{-# OPTIONS_GHC -W #-}
{-# LANGUAGE LambdaCase, BlockArguments, RecursiveDo #-}

module Regex.DFA where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Regex.Regex (Regex)
import Regex.NFA (NFA(..), Closure)
import qualified Regex.NFA as NFA
import Data.Maybe (catMaybes)
import Control.Monad.State

import Data.Map (Map)
import Data.List (elemIndex)

data DFA e = DFA Bool (Edges e) deriving (Show) 
type Edges e = Map.Map e (DFA e)
type Cache e = Map.Map (Closure e) (DFA e)


hasFinal :: (DFA e) -> Bool
hasFinal (DFA final _) = final


fromNFA :: (Ord e, Eq e) => (NFA e) -> (DFA e)
fromNFA nfa = evalState (conv $ NFA.closure nfa) Map.empty
  where
    -- Convert an NFA to a DFA within a state cache. The cache allows us to
    -- create circular references to the next DFA by keeping references to the
    -- other DFAs as the graph is being constructed.
    conv :: (Ord e) => (Closure e) -> State (Cache e) (DFA e)
    conv set = do
        cache <- get
        case Map.lookup set cache of
            -- DFA fragment previously computed, simply return.
            Just dfa -> pure dfa
            -- DFA fragment not computed. Build the DFA, and add to cache.
            Nothing -> build set

    -- Get the edges from the NFAs for each available symbol. See stepAll.
    -- Traverse is used to map the resulting edges of closures to edges of DFAs,
    -- and runs in the state cache.
    build :: (Ord e) => Closure e -> State (Cache e) (DFA e)
    build set = mdo
        let dfa = DFA (NFA.hasFinal set) edges
        modify $ Map.insert set dfa

        edges <- traverse conv $ stepAll set

        pure dfa

    -- For the given closure, find all outward transitions to the next closures
    stepAll :: (Ord e) => (Closure e) -> Map.Map e (Closure e)
    stepAll set = Map.fromList $ symClosure set <$> symsOf set
      where
        -- All of the unique symbols within this closure. Done to avoid having
        -- to guess all available characters.
        symsOf :: Closure e -> [e]
        symsOf = catMaybes . fmap symOf . Set.toList
          where
            symOf :: NFA e -> Maybe e
            symOf (Step _ sym _) = Just sym
            symOf _ = Nothing

        -- Where the work of creating the edges is done. For a given symbol and
        -- a closure, we calculate the closure of stepping with that symbol
        symClosure :: (Eq e) => Closure e -> e -> (e, Closure e)
        symClosure set sym = (sym, NFA.step sym set)


fromRegex :: (Ord e) => Regex e -> DFA e
fromRegex = fromNFA . NFA.fromRegex


run :: (Ord e) => [e] -> DFA e -> Maybe (DFA e)
run [] dfa = Just dfa
run (sym:syms) (DFA _ edges) = Map.lookup sym edges >>= run syms


match :: (Ord e) =>  DFA e -> [e] -> Bool
match dfa syms = case run syms dfa of
    Just dfa -> hasFinal dfa
    Nothing  -> False


matchRe :: (Ord e) =>  Regex e -> [e] -> Bool
matchRe = match . fromRegex

data BreakpointAST e = BreakpointAST {
  fanout :: [(Int, [( e, Int)])],
  accepting :: [Int]
} deriving Show

toBreakpointAST :: (Ord e) => DFA e -> BreakpointAST e
toBreakpointAST dfa = evalState (p dfa) ([], [], 0)


p :: (Ord e) => DFA e -> State ([(Int, [(e, Int)])], [Int], Int) (BreakpointAST e)
p dfa =  do 
  (fanout, accepting, currentId) <- get
  let isAccepting = dfaIsAccepting dfa
  modify (\(f, a, n) -> (f, if isAccepting then currentId : a else a, n))
  transitions <- mapM (processEdge) (Map.toList (getEdges dfa))
  modify (\(f, a, n) -> ((currentId, transitions) : f, a, n))

  gets (\(f, a, _) -> BreakpointAST f a) 
  

-- Helper to process each edge and recursively handle the target DFA
processEdge :: (Ord e) => (e, DFA e) -> State ([(Int, [(e, Int)])], [Int], Int) (e, Int)
processEdge (label, targetDfa) = do
  nextId <- gets (\(_, _, n) -> n + 1)
  modify (\(f, a, n) -> (f, a, nextId))  -- Increment the next ID
  -- recurse 
  p targetDfa
  -- update state with target changes 
  return (label, nextId)

-- Helper to determine if a DFA state is accepting
dfaIsAccepting :: DFA e -> Bool
dfaIsAccepting (DFA b _) = b

-- Get the edges from a DFA
getEdges :: DFA e -> Edges e
getEdges (DFA _ edges) = edges

toISLI :: BreakpointAST e -> (e -> Bool) -> 