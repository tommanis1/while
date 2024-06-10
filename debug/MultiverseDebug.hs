{-# LANGUAGE GADTs, MultiParamTypeClasses, RankNTypes, FlexibleContexts, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances #-}

module MultiverseDebug where

import qualified Data.Map as M
import qualified Control.Applicative as List
import Control.Monad (forM_)

-- data DebugState = DebugState Config Program
--     deriving (Eq)

-- instance Show DebugState where 
    -- show (DebugState _ p) = show p -- Temp

class Evaluate e c v where 
    estate :: e -> c -> v

class Reduce r c a | r c -> a where 
    rstate :: r -> c -> a
    

-- instance Reduce () Int Int where 
--     rstate _ c = c

-- instance Evaluate Int Int Bool where 
--     estate goal curr = goal == curr 

-- instance Reduce Integer Int Int where 
--     rstate _ = id


data STR c a = STR 
    { initial :: [c]
    , actions :: c -> [a] -- Having this here is kinda unfortunate, as it makes infinite actions difficult to represent.
    , execute :: c -> a -> [c] -- Would be nicer if we can change the [c] into m c for some monad and then change the concat to a join (noted below).
    }


-- strSimple :: STR Int String 
-- strSimple = STR 
--     { initial = [0]
--     , actions = const ["next"]
--     , execute = eval
--     }

data TR c a = TR 
    { tinitial :: [c]
    , accepting :: c -> Bool
    , next :: c -> [c] 
    }


data DebugConfig c = DebugConfig 
    { current :: Maybe c
    , history :: [c]
    , options :: [c]

    }
    deriving (  Eq)

-- Custom Show instance for DebugConfig
instance (Show c) => Show (DebugConfig c) where
    show DebugConfig {current = curr, history = hist, options = _} = 
        "current DebugState : " ++ show curr ++ ""

data DebugAction a c = Step a | Select c | Jump c | RunBreak
    deriving Show


type Finder c a e r alpha = (Evaluate e c Bool, Reduce r c alpha) => STR c a -> [c] -> e -> r -> [c]

simpleFinder :: Finder Int String String Int Int
simpleFinder _ cs _ _ = cs

strToTr :: (c -> Bool) -> STR c a -> TR c a 
strToTr accept str = TR 
    { tinitial = initial str
    , accepting = accept
    , next = \c -> concat [execute str c a | a <- actions str c] -- Here we would change the concat to a join. Not sure what is exactly means though.
    }

hasCurrent :: DebugConfig c -> Bool
hasCurrent c = case current c of 
    (Just _) -> True
    Nothing -> False

rmdActions :: STR c a -> DebugConfig c -> [DebugAction a c]
rmdActions str config = oa ++ sa ++ ja ++ [RunBreak]
    where 
        oa = case current config of 
            (Just c) -> [Step a | a <- actions str c]
            Nothing -> []
        sa = [Select c | c <- options config]
        ja = [Jump c | c <- history config]


-- We need the [DebugConfig c] to allow us to use the STR interface which expects a List. 
-- Nevertheless, it is either empty or has one element, so encoding the Maybe type.
rmdExecute :: (Evaluate e c Bool, Reduce r c alpha) => 
    STR c a -> Finder c a e r alpha -> e -> r -> DebugAction a c -> DebugConfig c -> [DebugConfig c]
rmdExecute o fnd brk red action config = 
    case action of 
        (Step a) -> case current config of 
            (Just c) -> [DebugConfig {current = Nothing, history=history config, options=execute o c a}]
            Nothing -> []
        (Select c) -> [DebugConfig {current=Just c, history=history config ++ [c], options=[]}] -- Select goed forward
        (Jump c) -> [DebugConfig {current=Just c, history=history config ++ [c], options=[]}] -- Jump goes backward. Double check: do these have the same effect?
        RunBreak -> case current config of
            (Just c) -> case fnd o [c] brk red of 
                [] -> []
                (h:t) -> [DebugConfig {current=Just h, history=history config ++ h:t, options=[]}]
            Nothing -> case fnd o (options config) brk red of 
                    [] -> []
                    (h:t) -> [DebugConfig {current=Just h, history=history config ++  h:t, options=[]}]



reducedMultiverseDebuggerBridge :: (Evaluate e c Bool, Reduce r c alpha) => STR c a -> Finder c a e r alpha -> e -> r -> STR (DebugConfig c) (DebugAction a c)
reducedMultiverseDebuggerBridge o fnd brk red = 
    STR { initial = rmdInitial
        , actions = rmdActions o
        , execute = flip (rmdExecute o fnd brk red)
        }
    where 
        rmdInitial = [DebugConfig {current = Nothing, history=[], options=initial o}]


searchBreakpoints :: Eq alpha => TR c a -> (c -> alpha) -> [c]
searchBreakpoints o reduce = 
    let k = []
        wt = [] 
    in loop wt k (tinitial o)
    where 
        loop wt _ [] = wt
        loop wt k (s:hs) = case dfs o reduce s k wt of 
            (wt', _, True) -> wt'
            (wt', k', False) -> loop wt' k' hs


dfs :: Eq alpha => TR c a -> (c -> alpha) -> c -> [alpha] -> [c] -> ([c], [alpha], Bool)
dfs o reduce s k wt 
    | accepting o s = (s:wt, k, True)
    | otherwise = let k' = k ++ [reduce s]
        in  case filter found . dfsDeeper k' . filter (reducedSeen k') $ next o s of 
                (h:_) -> h -- Always grab the first?
                [] -> ([], [], False) 
            where 
                reducedSeen k' c = let rc = reduce c in notElem rc k'
                dfsDeeper k' = map (\t -> dfs o reduce t k' (s:wt))
                found (_, _, b) = b


finder :: Eq alpha => Finder c a e r alpha -- (Evaluate e c Bool, Reduce r c alpha, Show a, Show c) => STR c a -> Finder c a e r alpha 
finder o cs e r = searchBreakpoints tr reducer
    where 
        reducer = rstate r
        tr = strToTr (estate e) o {initial = cs}


-- Instead of Show a, Show c, take a pretty print instances to improbe the printing of the configurations.
debugLoop :: (Evaluate e c Bool, Reduce r c alpha, Show a, Show c) => STR c a -> STR (DebugConfig c) (DebugAction a c) -> DebugConfig c -> Finder c a e r alpha -> e -> r -> IO () 
debugLoop o doo c fnd brek red = do 
    -- The way a action is chosen is very rudimentary right now. Need to find a better approach.
    let aact = actions doo c 
    -- putStrLn "Current state:"
    -- print c
    putStrLn "Available actions:"
    forM_ (zip [0..] aact) $ \(i, act) -> putStrLn $ show i ++ ": " ++ show act
    choice <- getLine
    let choiceInt = read choice
    let tempc = rmdExecute o fnd brek red (aact !! choiceInt) c
    let newc = if length tempc > 0 then head tempc else DebugConfig Nothing [] [] -- Keep this or go back to previous? What to do?
    debugLoop o doo newc fnd brek red

debugger :: (Evaluate e c Bool, Reduce r c alpha, Show a, Show c) => STR c a  -> Finder c a e r alpha -> e -> r -> IO () 
debugger o fnd brek red = 
    let doo = reducedMultiverseDebuggerBridge o fnd brek red
        in debugLoop o doo (head . initial $ doo) fnd brek red
    -- let doo = reducedMultiverseDebuggerBridge o fnd brek red
    -- print (head . tail $ actions doo (head . initial $ doo))
    -- let newdb = head $ rmdExecute o fnd brek red RunBreak (head $ initial doo)
    -- print newdb
    -- print (actions doo newdb)


-- This does no reduction and uses == to find breakpoints, in this case it searches for the state identified by 2.
-- Interact with the debugger by giving the list index of the action you want to execute (0th based).
-- exampleDebugger :: IO ()
-- exampleDebugger = debugger strSimple finder (2 :: Int) ()


