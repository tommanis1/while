{-# LANGUAGE ScopedTypeVariables
 #-}
{-# LANGUAGE JavaScriptFFI, GADTs, MultiParamTypeClasses, RankNTypes, FlexibleContexts, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances #-}
module Main where

import DebugWhile
import While

import MVD.Debugger
import MVD.Finders
import MVD.Interface
import GHC.JS.Foreign.Callback
import GHC.JS.Prim
import Data.IORef


import qualified Programs.IncorrectMutex2 as P2

import Examples 
main :: IO ()
main = 
    debuggerGUI (whileStr  P2.p) (finder bfsStepper emptyPruner singleBreaker ) Examples.mutex_violation ""
foreign import javascript "wrapper"
  js_func_from_hs :: Int -> IO JSVal


-- Minimal example of receiving a value from JS
-- import GHC.JS.Foreign.Callback 
-- import GHC.JS.Prim
-- import Data.IORef


-- --sort of FFI export
-- foreign import javascript "((a) => { globalTakeUserAction = a })"
--   setA ::Callback (JSVal -> IO JSVal) -> IO JSVal


-- handler :: IORef [String] -> JSVal -> IO JSVal
-- handler l v = do
--   l' <- readIORef l
--   let i = fromJSInt v
--   consoleLog $ toJSString $  "list was: " ++ show l'
--   consoleLog $ toJSString $ "choice: " ++ show i
--   consoleLog $ toJSString $ "new list is: " ++ show [ l' !! i]
--   consoleLog $ toJSString $ ""

--   modifyIORef' l (\x -> x ++ ["a", "b"])
--   return v


-- main = do 
--     t "test"
-- t _ = do 
--     let x :: [String] = ["a"]
--     r <- newIORef x

--     let m_log ::  IO (Callback (JSVal -> IO JSVal)) =  syncCallback1' (Main.handler r ) 
--     log <- m_log
    
--     let mi_a :: IO Int = fromJSInt <$> Main.setA log
--     i_a <- mi_a
--     print i_a