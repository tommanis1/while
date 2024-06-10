{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module DebugWhile where
import qualified Data.Map as M
import qualified Control.Applicative as List
import Data.Maybe
import MultiverseDebug
import While hiding (And)


data DebugState = DebugState Config (Maybe Command)
    deriving (Eq)

data BreakPoint = And BreakPoint BreakPoint
    | Or BreakPoint BreakPoint
    | VariableCondition String (Literal -> Bool)
    | OutputCoutingFromLatest Int String

instance Evaluate BreakPoint DebugState Bool where
    estate :: BreakPoint -> DebugState -> Bool
    estate b (DebugState (Config { cfgEnvironment = Environment store threads, cfgOutput = out } ) cmd)  = lookup b
    
        where lookup b = case b of 
                (And b1 b2 ) -> lookup b1 && lookup b2
                (Or b1 b2) -> lookup b1 || lookup b2
                (VariableCondition i f) -> case M.lookup i store of
                    Just v -> f v
                    _      -> False
                (OutputCoutingFromLatest i s) -> if length out < i + 1 then False else
                    case out !! ((length out) - 1 ) of
                        s -> True
                        _ -> False

example_breakpoint :: BreakPoint
example_breakpoint = And (And (int_val_breakpoint "bal" ((==) 0)) (int_val_breakpoint "tmp1" ((==) 1))) (int_val_breakpoint "tmp2" ((==) 1))

int_val_breakpoint :: String -> (Integer -> Bool) -> BreakPoint
int_val_breakpoint n c = VariableCondition n (\case 
    LitInt x -> c x
    _ -> False )

                

-- instance Evaluate () DebugState Bool where
--     estate :: () -> DebugState -> Bool
--     estate () (DebugState (Config { cfgEnvironment = Environment store threads, cfgOutput = _ } ) cmd) = 
--         cmd == Nothing && null threads

instance Reduce () DebugState DebugState where
    rstate () dbgState = dbgState -- Simple identity reduction for this example

instance Show DebugState where 
    show (DebugState config command) = 
        "DebugState{Config: " ++ compactShow config ++ ", Command: " ++ compactShow command ++ "}"

compactShow :: (Show a) => a -> String
compactShow = unwords . words . show

{- whileExecute :: DebugState -> String -> [DebugState]
whileExecute (DebugState cfg cmd) action =
    case action of
        "step" ->
            case cmd of 
                (Seq Done c2) -> [DebugState cfg cmd]
                Done -> []
                (Seq c1 c2) -> do


        _ -> error "Unsupported action"
 -}
whileExecute :: DebugState -> String -> [DebugState]
whileExecute (DebugState cfg cmd) action =
    case action of
        "step" -> mapMaybe createDebugState (definterpStepNonDeterm cfg cmd)
        _ -> error "Unsupported action"

createDebugState :: (Maybe Config, Maybe Command) -> Maybe DebugState
createDebugState (Just cfg, cmd) = Just (DebugState cfg cmd)
-- createDebugState (Just cfg, _) -- TODO done condition ? 
createDebugState _ = Nothing


whileStr p = STR 
    { initial = [DebugState emptyConfig p]
    , actions = const ["step"]
    , execute = whileExecute
    }


debug :: Command -> BreakPoint -> IO ()
debug c b = debugger (whileStr (Just c)) finder b ()
