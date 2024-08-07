{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE CPP #-}

module DebugWhile where
import qualified Data.Map as M
import qualified Control.Applicative as List
import Data.Maybe
import While hiding (And)
import Control.Monad.Reader

import MVD.Debugger
import MVD.Finders
import MVD.Interface

import Data.List (intercalate)

#ifdef __GHCJS__
import GHC.JS.Prim
#endif

data DebugState = DebugState Config (Maybe Command)
    deriving (Eq)

data BreakPoint = 
    And BreakPoint BreakPoint
    | U_And [BreakPoint]
    | Or BreakPoint BreakPoint
    | Equal BreakpointCondition BreakpointCondition
    | C BreakpointCondition

data BreakpointCondition = 
    E Expr
    | InStore String
    | OutputCoutingFromLatest Int
    -- | IsTrue String


-- Actions are made up of an integer for the id of the thread that is taking a step. Together with the config that results from choosing that action.
data WhileAction = WhileAction {threadId :: Integer}
instance (Show WhileAction) where show a = "Step thread " ++ show (threadId a)
{- data BreakPoint = 
    And BreakPoint BreakPoint
    | U_And [BreakPoint]
    -- | U_OR [BreakPoint]
    | Or BreakPoint BreakPoint
    | Not BreakPoint
    | Equal BreakpointCondition BreakpointCondition


data BreakpointCondition = 
    TargetConfiguration SubjectCondition |
    CurrentConfiguration SubjectCondition

data SubjectCondition = 
    Expr
    | OutputCoutingFromLatest Int

-- Turnchanged from listing 5 from Temporal Breakpoints for Multiverse Debugging
example :: BreakPoint
example = Not $ Equal (TargetConfiguration (Id "turn")) (CurrentConfiguration (Id "turn"))

ISTRwhile = ISTR {
    inital = [0]
    , actions = \ i c -> 
} -}





instance Evaluate BreakPoint Config Bool where
    estate :: BreakPoint -> Config -> Bool
    estate b (Config cmd store out threads)  = lookup b
    
        where 
            lookup b = case b of 
                (And b1 b2 ) -> lookup b1 && lookup b2
                
                (U_And []) -> False
                (U_And [b]) -> lookup b
                (U_And [b1, b2]) -> lookup b1 && lookup b2
                (U_And( b1:b2:xs)) -> lookup b1 && lookup b2 && lookup (U_And xs)
                (Or b1 b2) -> lookup b1 || lookup b2
                (Equal bc1 bc2) -> (evalBreakpointCondition bc1) == (evalBreakpointCondition bc2)
                (C c) -> compare (evalBreakpointCondition c) 
            
            compare (LitBool b) = b
            compare _ = error "not bool"

            -- Evaluate a breakpoint expression to a Literal in while. This is not ideal, say we want to define a BreakpointCondition over the threads entity. We can't express Command as a Literal in while. So that woudl require either adding Command as a Literal or defining some other type that enumerates literals and commands.
            evalBreakpointCondition :: BreakpointCondition -> Literal
            evalBreakpointCondition bc = case bc of
                (OutputCoutingFromLatest i) -> 
                    if length out < i + 1  then 
                        LitBool False else  LitString $ out !! (((length out) - 1 ) - i)
                (InStore s) -> LitBool $ elem s $ M.keys store

                (E e) -> exprToLiteral $ runReader (evalExpr' e) store
  
                            
            exprToLiteral :: Expr -> Literal
            exprToLiteral e = case e of
                (LitExpr l) -> l
                _ -> error "Not a literal"

-- example_breakpoint :: BreakPoint
-- example_breakpoint = And (And (int_val_breakpoint "bal" ((==) 0)) (int_val_breakpoint "tmp1" ((==) 1))) (int_val_breakpoint "tmp2" ((==) 1))

-- int_val_breakpoint :: String -> (Integer -> Bool) -> BreakPoint
-- int_val_breakpoint n c = VariableCondition n (\case 
--     LitInt x -> c x
--     _ -> False )

                

-- instance Evaluate () DebugState Bool where
--     estate :: () -> DebugState -> Bool
--     estate () (DebugState (Config { cfgEnvironment = Environment store threads, cfgOutput = _ } ) cmd) = 
--         cmd == Nothing && null threads

instance Reduce String Config Config where
    rstate _ c = c -- Simple identity reduction for this example

-- instance Show DebugState where 
--     show (DebugState config command) = 
--         "DebugState{Config: " ++ compactShow config ++ ", Command: " ++ compactShow command ++ "}"

-- compactShow :: (Show a) => a -> String
-- compactShow = unwords . words . show



whileStrSymbolicAction :: Command -> STR Config String String
whileStrSymbolicAction p = STR
    { initial = [wrapProgram p]
    , actions = const ["step"]
    , execute = \ c _ -> step c
    , commands = \ _ -> []
    , perform = \ c _ -> c
    }


whileStr :: Command -> STR Config WhileAction String
whileStr p = STR 
    { initial = [wrapProgram p]
    , actions = \ c -> map (\(id, _) -> WhileAction id) (filter ((/= Done) . snd) $  (While.current c : cfgThreads c))
    , execute = \ c a -> filter ((== threadId a) . fst . While.current) $ step c 
    , commands = \ _ -> []
    , perform = \ c _ -> c
    }


debug :: Command -> BreakPoint -> IO ()
debug c b = debuggerWithShow (whileStr c) (finder dfsStepper emptyPruner singleBreaker ) b ""

#ifdef __GHCJS__
foreign import javascript unsafe "create_other_entities_display"
  render_entities :: JSVal -> IO ()

enitiestoHtml :: Store -> Output -> String
enitiestoHtml store output = 
    "<div>" ++ storeHtml ++ outputHtml ++ "</div>"
  where
    storeHtml = "<div id='store'>" ++ (intercalate "<br>" (map showStoreEntry (M.toList store))) ++ "</div>"
    outputHtml = "<div id='output'>" ++ (intercalate "<br>" output) ++ "</div>"
    showStoreEntry (key, value) = key ++ ": " ++ show value

instance DebugGUI Config WhileAction where
    draw_current (Config cur store out threads) a = do
        consoleLog $ toJSString "error"
        let htmlContent = enitiestoHtml store out
        render_entities (toJSString htmlContent)
        
        -- return ()
    -- draw_history cs a = return ()
#endif
