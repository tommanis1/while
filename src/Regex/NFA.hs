{-# OPTIONS_GHC -W #-}
{-# LANGUAGE LambdaCase, BlockArguments, RecursiveDo #-}

module Regex.NFA where

import Control.Monad.State
import qualified Data.Set as Set
import Regex.Regex (Regex (..))
import Data.Maybe

data NFA e
    = Split Int (NFA e) (NFA e)
    | Step Int e (NFA e)
    | Final Int
    deriving Show

instance Eq (NFA e) where
  x == y = ident x == ident y

instance Ord (NFA e) where
  compare x y = compare (ident x) (ident y)


-- Get the identifier of this NFA node
ident :: NFA e -> Int
ident (Split i _ _) = i
ident (Step i _ _) = i
ident (Final i) = i


fromRegex :: Regex e -> NFA e
fromRegex re = let (nfa, (i, _)) = runState (conv re) (0, Final i) in nfa
  where
    -- Produce a unique id
    inc :: State (Int, (NFA e)) Int
    inc = state \(i, nfa) -> (i, (i + 1, nfa))

    -- Get the next node
    getNext :: State (Int, NFA e) (NFA e)
    getNext = get >>= pure . snd

    -- Set the next node
    putNext :: NFA e -> State (Int, NFA e) ()
    putNext nfa = modify \(i, _) -> (i, nfa)

    -- Helper function to build the Star and Plus graph nodes. These are
    -- constructed in the same way, the difference being that Star starts at the
    -- the split, and Plus starts at the NFA. Return both, and differentiate in
    -- conv.
    mkStar :: Regex e -> State (Int, NFA e) (NFA e, NFA e)
    mkStar re = mdo
        split <- Split <$> inc <*> getNext <*> pure nfa
        nfa   <- putNext split >> conv re
        pure (split, nfa)

    conv :: Regex e -> State (Int, NFA e) (NFA e)
    conv (Empty)     = getNext
    conv (Lit c)     = Step <$> inc <*> pure c <*> getNext
    conv (Or re re') = Split <$> inc <*> conv re <*> conv re'
    conv (Mark re)   = Split <$> inc <*> conv re <*> getNext
    conv (Star re)   = mkStar re >>= pure . fst
    conv (Plus re)   = mkStar re >>= pure . snd
    -- The And case is a bit tricky: in order to get the "intuitive" order of
    -- the ids, where the left regex has a smaller id than the right, we build
    -- the right node lazily, and pass it to the left.
    conv (And re re') = mdo
        next  <- getNext
        left  <- putNext right >> conv re -- Left conversion
        right <- putNext next >> conv re' -- Right conversion
        pure left

type Closure e = Set.Set (NFA e)


closure :: NFA e -> Closure e
closure = execClosure . closureST


execClosure :: State (Closure e) a -> (Closure e)
execClosure = flip execState Set.empty


closureST :: NFA e -> State (Closure e) ()
closureST nfa = do
    nfas <- get
    if Set.member nfa nfas then pure () else do
        modify $ Set.insert nfa
        recurse nfa
  where
    recurse (Split _ l r) = closureST l >> closureST r
    recurse _ = pure ()


step :: (Eq e) => e -> (Closure e) -> (Closure e)
step sym set = execClosure $ traverse closureST $ stepAll set
  where
    -- stepAll :: Closure e -> [NFA e]
    stepAll = catMaybes . (stepOne sym <$>) . Set.toList

    -- stepOne :: (Eq e) => e -> NFA e -> Maybe (NFA e) 
    stepOne sym (Step _ sym' nfa)
      | sym == sym' = Just nfa
      | otherwise   = Nothing
    stepOne _ _ = Nothing


run :: (Eq e) => [e] -> NFA e -> (Closure e)
run syms nfa = go syms $ closure nfa
  where
    go :: (Eq e) => [e] -> (Closure e) -> Closure e
    go [] set         = set
    go (sym:syms) set = go syms $ step sym set


hasFinal :: Closure e-> Bool
hasFinal set = go $ Set.toList set
  where
    go :: [NFA e] -> Bool
    go [] = False
    go (Final _:_) = True
    go (_:nfas) = go nfas


match :: (Eq e) => NFA e -> [e] -> Bool
match nfa syms = hasFinal $ run syms nfa


matchRe :: (Eq e) => Regex e -> [e] -> Bool
matchRe = match . fromRegex