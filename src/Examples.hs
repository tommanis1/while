{-# LANGUAGE FlexibleContexts #-}
module Examples where

import qualified Programs.IncorrectMutex1 as P1
import qualified Programs.IncorrectMutex2 as P2
import qualified Programs.IncorrectMutex3 as P3
import qualified Programs.IncorrectMutex4 as P4
import qualified Programs.VariableChanges
import qualified Programs.DiningPhilosophers as DiningPhilosophers


import DebugWhile
import MVD.Debugger
import MVD.Finders
import MVD.Interface

import SmartConstructorsWhile
import While

import qualified Data.Map as Map
 
{- example_trivial0 = debuggerWithShow (whileStr (Just $ Seq (Print . LitExpr . LitString $ "t0 0") (Print . LitExpr . LitString $ "t0 1"))) equalityFinder (OutputCoutingFromLatest 0 "t0 1") ()
 -}

-- example_trivial1 = debuggerWithShow whileStr (Just (Seq (Thread(Seq (print "t0 0") (print "t0 1"))) (Thread(Seq (print "t1 0") (print "t1 1"))))) equalityFinder () ()

-- In the implementation IncorrectMutex1 two threads are forced to alternate, ensuring mutual exclusion, but preventing the same thread from entering its critical section consecutively.
{- example_Mutex1 = debuggerWithShow (whileStr (Just P1.p)) equalityFinder (
    Or
    (U_And [ 
        (OutputCoutingFromLatest 0 "End critical section 0 ") 
        , (OutputCoutingFromLatest 1 "Begin critical section 0 ")
        , (OutputCoutingFromLatest 2 "End critical section 0 ")
        ]    
    )
    (U_And [ 
        (OutputCoutingFromLatest 0 "End critical section 0 ") 
        , (OutputCoutingFromLatest 1 "Begin critical section 0 ")
        , (OutputCoutingFromLatest 2 "End critical section 0 ")
        ]
    )) ()
example_Mutex1' = debuggerWithShow (whileStr (Just P1.p)) equalityFinder (
    (
        (OutputCoutingFromLatest 0 "End critical section 00 ") 
          
    )) () -}
no_mutual_exclusion_breakpoint :: BreakPoint 
no_mutual_exclusion_breakpoint = Or
    (DebugWhile.And 
        (Equal (OutputCoutingFromLatest 0) (E $ LitExpr .LitString $ "Begin critical section 0 "))
        (Equal (OutputCoutingFromLatest 0) (E $ LitExpr .LitString $ "Begin critical section 1 ")) 
    )
    (DebugWhile.And 
        (Equal (OutputCoutingFromLatest 1) (E $ LitExpr .LitString $ "Begin critical section 0 "))
        (Equal (OutputCoutingFromLatest 0) (E $ LitExpr .LitString $ "Begin critical section 1 ")) 
    )




-- example_Mutex2 = debug_mutex_example' P2.p (Equal (OutputCoutingFromLatest 0) (E $ LitExpr .LitString $ "Begin critical section 0 "))  
-- example_Mutex3 = debug_mutex_example P3.p
-- example_Mutex4 = debug_mutex_example P4.p

-- example_naive_dinining_philosophers =

--     debuggerWithShow ( whileStr $ Just DiningPhilosophers.naive) equalityFinder (
--         U_And [
--             (Equal (OutputCoutingFromLatest 0) (E $ LitExpr .LitString $ "philosopher 2 acquired fork 2"))
--             , (Equal (OutputCoutingFromLatest 1) (E $ LitExpr .LitString $ "philosopher 1 acquired fork 1"))
--             , (Equal (OutputCoutingFromLatest 2) (E $ LitExpr .LitString $ "philosopher 0 acquired fork 0"))
--         ]
--         ) ()
{- -- TEst
test = debug_mutex_example' (  Seq (Print . LitExpr . LitString $ "s")(Print . LitExpr . LitString $ "s")) (Equal (E $ Id "claimed_0") (E $ LitExpr .LitBool $ True))  


tt = whileExecute (DebugState (emptyConfig) (Just $ Print . LitExpr . LitString $ "ss")) ""

ttt = step (Print . LitExpr . LitString $ "ss") emptyConfig
-}
no_mutex = 
    U_And [
    U_And [
        C $ InStore "critical_0"
        , C $ InStore "critical_1"]
    -- ,
    -- , C $ E $ Id $ "critical_1"]
    ,
        C $ E (Eq (Id $ "critical_0") (Id "critical_1"))


    -- C $ E $ And (Eq (Id $ "critical_0") (Id "critical_1")) (Eq (Id "critical_1") (b True))
    ]

no_mutex1 = U_And [
    C $ InStore "i0"
    ,     C $ E (Eq (Id "i0") (i 3))

    ]

mutex_violation = U_And[
         C $ InStore "critical_1"
        --  , C $ IsTrue "claimed_1"
        --  , C $ IsTrue "claimed_0"
        , Equal  (E $ Id $ "critical_1") (E $ b $ True)
        , Equal  (E $ Id $ "critical_0") (E $ b $ True)

        ]

incorrect_mutex_2DFS = debug P2.p mutex_violation

incorrect_mutex_2 = debuggerWithShow (whileStr  P2.p) (finder bfsStepper emptyPruner singleBreaker ) mutex_violation ""



mutex_3 = debuggerWithShow (whileStr  P3.p) (finder bfsStepper emptyPruner ( multiStateBreakpointBreakerBegin deadlock)) mutex_violation ""
deadlock :: Tree ( (Config, Config) -> Bool )
deadlock =  TreeNode two_threads_checking$ [
    TreeNode (\_-> True) [TreeNode two_threads_checking []]  ]
    -- TreeNode two_threads_checking [TreeNode two_threads_checking []] ]
    where 
    --     two_threads_waiting = \((Config c _ _ t), _) -> 2 == (length [ 1 | (_, Seq Done (While _ _ Done)) <- c:t])
        two_threads_checking =  \((Config c _ _ t), _)-> (length $ [ 1 | (_, Seq (Seq(While _ _ Done ) _) _) <- c:t]) >= 2 --(While _ _ Done)

-- Step breakpoints 
val_changes id =  \((Config _ store _ _), _)->
    let 
        v1 = id `lookup_val_in` store
        right v1= \((Config _ store _ _), _)-> 
            let v2 = id `lookup_val_in` store in 
                case v2 of 
                    Nothing -> False
                    (Just v2) -> v2 /= v1
    in case v1 of 
        Nothing -> \_-> False
        (Just v1) -> right v1


    where 
        lookup_val_in :: String-> Store -> Maybe Literal
        lookup_val_in var store = Map.lookup var store


value_changes = debuggerWithShow (whileStr Programs.VariableChanges.p) (finder bfsStepper emptyPruner (stepBreakpointBreaker (val_changes "x"))) mutex_violation ""