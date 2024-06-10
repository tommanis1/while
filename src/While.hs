module While where

import qualified Data.Map as Map
import Data.List
import Data.Maybe
-- import Data.Graph.Inductive (emap)
import Control.Monad.Trans.Writer.Lazy
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import Control.Monad.Identity
-- import qualified Language.Explorer.Basic as E
-- import qualified Language.Explorer.Pure as EP
-- import qualified Language.Explorer.Monadic as EM
-- import qualified Language.Explorer.Tools.REPL as R
-- import Language.Explorer.Basic (mkExplorerNoSharing)

import Debug.Trace


data Literal = LitBool Bool 
              | LitInt Integer
              | LitString String
  deriving (Eq, Read)

instance Show Literal where
    show (LitBool b) = show b
    show (LitInt i) = show i
    show (LitString s) = s


data Expr = Eq Expr Expr
          | Leq Expr Expr
          | Plus Expr Expr
          | LitExpr Literal
          | And Expr Expr
          | Id String deriving (Eq, Read)

instance Show Expr where
    show (Eq e1 e2) = show e1 ++ "==" ++ show e2

    show (Leq e1 e2) = show e1 ++ "<=" ++ show e2
    show (Plus e1 e2) = show e1 ++ "+" ++ show e2
    show (LitExpr lit) = show lit
    show (And e1 e2) = show e1 ++ "&&" ++ show e2
    show (Id s) = s

data Command = Seq Command Command 
    | Assign String Expr 
    | Print Expr
    | While Expr Expr Command
    | Done
    | Thread Command
    deriving (Eq, Read)

instance Show Command where
    show (Print e1) = "print(" ++ show e1 ++ ")"
    show Done = "Done"
    show (Assign s e) = s ++ " = " ++ show e
    show (Seq c1 c2) = "Seq( " ++ show c1 ++ "\n" ++ show c2 ++ ")"
    show (While e1 e2 c) = "while(" ++ show e2 ++ ") do\n" ++ show c ++ "\nod"
    show (Thread c) = "new thread " ++ show c ++ " \n"

emptyEnvironment :: Environment
emptyEnvironment = Environment { store = Map.empty, threads = [] }
emptyConfig :: Config
emptyConfig = Config { cfgEnvironment = emptyEnvironment, cfgOutput = [] }


-- Ideally nested environments 
{- 
data Environment = Environment
  { 
    store :: Map.Map String Literal
  , threads :: [Environment]

Problem is how do you traverse up the stores? 
 -}

data Environment = Environment
  {
    store :: Map.Map String Literal
  , threads :: [Command]
  }

-- type Store = Map.Map String Literal
-- type StoreM = State Store

instance Show Environment where
    show (Environment st th) = "{ store = " ++ show st ++ ", threads = " ++ show th ++ " }"

instance Eq Environment where
    (Environment st1 th1) == (Environment st2 th2) = st1 == st2 && th1 == th2

type EnvironmentM = State Environment

type Output = [String]



-- data Config = Config { cfgStore :: Store, cfgOutput :: Output, cfgThreadMap :: ThreadMap } deriving (Show, Eq)
data Config = Config { cfgEnvironment :: Environment, cfgOutput :: Output } deriving (Show, Eq)

-- type WhileExplorer = E.Explorer Command Config
-- type WhileExplorerM = EM.Explorer Command IO Config ()
-- type WhileExplorerO = EP.Explorer Command Config [String]

evalPlus :: Expr -> Expr -> EnvironmentM Expr
evalPlus (LitExpr (LitInt l1)) (LitExpr (LitInt l2)) = return $ LitExpr $ LitInt (l1 + l2)
evalPlus (LitExpr l1) l2 = do
    l2' <- evalExpr l2
    return (Plus (LitExpr l1) l2')
evalPlus l1 l2 = do
    l1' <- evalExpr l1
    return (Plus l1' l2)


evalLeq :: Expr -> Expr -> EnvironmentM Expr
evalLeq (LitExpr (LitInt l1)) (LitExpr (LitInt l2)) = return $ LitExpr $ LitBool (l1 <= l2)
evalLeq (LitExpr l1) e2 = do
    e2' <- evalExpr e2
    return (Leq (LitExpr l1) e2')
evalLeq e1 e2 = do
    e1' <- evalExpr e1
    return (Leq e1' e2)

evalEq :: Expr -> Expr -> EnvironmentM Expr
evalEq (LitExpr (LitInt l1)) (LitExpr (LitInt l2)) = return $ LitExpr $ LitBool (l1 == l2)
evalEq (LitExpr l1) e2 = do
    e2' <- evalExpr e2
    return (Eq (LitExpr l1) e2')
evalEq e1 e2 = do
    e1' <- evalExpr e1
    return (Eq e1' e2)

evalAnd :: Expr -> Expr -> EnvironmentM Expr
evalAnd e@(LitExpr (LitBool False)) _ = return e

evalAnd (LitExpr (LitBool b1)) (LitExpr (LitBool b2)) = return $ LitExpr $ LitBool (b1 && b2)
evalAnd (LitExpr l1) e2 = do
    e2' <- evalExpr e2
    return (And (LitExpr l1) e2')
evalAnd e1 e2 = do
    e1' <- evalExpr e1
    return (And e1' e2)

evalExpr :: Expr -> EnvironmentM Expr
evalExpr (LitExpr e) = return $ LitExpr e
evalExpr (Plus e1 e2) = evalPlus e1 e2
evalExpr (Leq e1 e2) = evalLeq e1 e2
evalExpr (Eq e1 e2) = evalEq e1 e2

evalExpr (And e1 e2) = evalAnd e1 e2
evalExpr (Id s) = do
    env <- get
    let (Environment store threads) = env
    let l = Map.lookup s store
    case l of
        Just lit -> return $ LitExpr lit
        Nothing -> error $ "Invalid Id: " ++ s

evalExpr' :: Expr -> EnvironmentM Expr
evalExpr' (LitExpr e) = return $ LitExpr e
evalExpr' e = do
    e' <- evalExpr e
    evalExpr' e'

storeInStore :: String -> Expr -> EnvironmentM Command
storeInStore s (LitExpr l) = do
    env <- get
    let (Environment lut threads) = env
    put $ Environment (Map.insert s l lut) threads
    return Done


spawnThread :: Command -> EnvironmentM Command
spawnThread c = do
    env <- get
    let (Environment store threads) = env
    put $ Environment store (threads ++ [c])

    return Done

evalCommand :: Command -> WriterT [String] EnvironmentM Command
evalCommand (Print e) = do
    x <- lift $ evalExpr' e
    tell [show x]
    return Done

evalCommand Done = return Done

evalCommand (Assign id e) = do
    exprValue <- lift $ evalExpr' e -- Evaluate the expression
    case exprValue of
        LitExpr lit -> do
            lift $ modify (\env -> let (Environment store threads) = env in Environment (Map.insert id lit store) threads) -- Update the environment
            return Done
        _ -> error "Expression did not evaluate to a literal."

evalCommand (Seq Done c2) = return c2
evalCommand (Seq c1 c2) = do
    c1' <- evalCommand c1
    return $ Seq c1' c2

evalCommand (While (LitExpr (LitBool False)) e2 c) = return Done
evalCommand (While (LitExpr (LitBool True)) e2 c) = return $ Seq c (While e2 e2 c)
evalCommand (While e1 e2 c) = do
    e1' <- (lift . evalExpr') e1
    return $ While e1' e2 c

evalCommand (Thread c) = do
    lift $ spawnThread c  -- Use lift to lift the computation into WriterT [String] EnvironmentM


evalCommand' :: Command -> WriterT [String] EnvironmentM Command
evalCommand' Done = return Done
evalCommand' c = do
    c' <- evalCommand c
    evalCommand' c'

-- Initial configuration in the while language)
initialConfig :: Config
initialConfig = Config {cfgEnvironment = Environment{store=Map.empty, threads=[]}, cfgOutput = []}

--  what do these three do??
-- Definitial interpreter for the while language.
definterp :: Command -> Config -> Maybe Config
definterp c cfg = Just cfg { cfgEnvironment = newenv, cfgOutput = cfgOutput cfg ++ newout }
  where
    Environment store threads = cfgEnvironment cfg
    (newout, newenv) =
      if null threads
        then
            let  ((_, newout'), newenv') = runState (runWriterT (evalCommand' c)) (cfgEnvironment cfg) in
                (newout' ,newenv')
        else
          let
            ((_, newout'), newenv') = runState (runWriterT (evalCommand' (head threads))) (cfgEnvironment cfg)
            Environment store' threads' = newenv'
            newenv = Environment store' (threads' ++ [c])
          in
            ( newout', newenv)
    _ = trace ("Number of threads: " ++ show (length threads)) ()


definterpO c cfg = (Just $ cfg {cfgEnvironment = newenv}, newout)
    where
        ((_, newout), newenv) = runState (runWriterT (evalCommand' c)) (cfgEnvironment cfg)
        _ = trace ("cfg: " ++ show (cfg)) ()


-- Simulate doing IO in the definitional interpreter.
definterpM :: Command -> Config -> IO (Maybe Config, ())
definterpM c cfg = do
    -- let _ = trace ("Number of threads: " ++ show (length threads)) ()  -- Debugging line
    -- putStrLn "test"
    mapM putStrLn newout
    return (Just $ cfg {cfgEnvironment = newenv, cfgOutput = []}, ())
      where
    Environment store threads = cfgEnvironment cfg
    (newout, newenv) =
      if null threads
        then
            let  ((_, newout'), newenv') = runState (runWriterT (evalCommand' c)) (cfgEnvironment cfg) in
                (newout' ,newenv')
        else
          let
            ((_, newout'), newenv') = runState (runWriterT (evalCommand' (head threads))) (cfgEnvironment cfg)
            Environment store' threads' = newenv'
            newenv = Environment store' (tail threads' ++ [c])
          in
            ( newout', newenv)

-- parser :: String -> c -> Maybe Command
-- parser s _ = Just (read s :: Command)

-- repl = R.repl (const "While> ") parser ":" R.metaTable  (\_ ex -> return ex) (\_ -> return ()) (EM.mkExplorerNoSharing definterpM initialConfig)


-- start :: IO WhileExplorer
-- start = return whileExplorer

-- startM :: IO WhileExplorerM
-- startM = return whileTree

-- startO :: WhileExplorerO
-- startO = whileGraphO




-- -- When using sharing, this results in 3 configurations and not 4,
-- -- since the IO effect is hidden in the monad and not part of the
-- -- configurations anymore.
-- session2 :: IO WhileExplorerM
-- session2 = startM >>=
--   do_2 (assign "x" (intToExpr 1)) >>=
--   do_2 (assign "y" (Id "x")) >>=
--   do_2 (Print (Id "y"))


-- session3 :: (WhileExplorerO, [String])
-- session3 =
--   do_3 (Print (Id "y")) $ do_3 (assign "y" (Id "x")) $ do_3 (assign "x" (intToExpr 1)) (startO, [])

-- Below are some helpers to create a Command and fully evaluate it.
-- Example:
-- ghci> let x = wprint (intToExpr 10) `wseq` (wprint (intToExpr 100) `wseq` wprint (intToExpr 200))
-- ghci> runCommand' x
-- ["10","100","200"]
-- ghci>
-- runCommand :: Command -> IO()
-- runCommand c = do
--     let ((_, output), env) = runState (runWriterT (evalCommand' c)) ( Environment Map.empty [])
--     print output
--     print env


-- intToExpr :: Integer -> Expr
-- intToExpr = LitExpr . LitInt

-- boolToExpr :: Bool -> Expr
-- boolToExpr = LitExpr . LitBool

-- while ::  Expr -> Command -> Command
-- while e = While e e

-- leq :: Expr -> Expr -> Expr
-- leq = Leq

-- wprint :: Expr -> Command
-- wprint = Print

-- plus :: Expr -> Expr -> Expr
-- plus = Plus

-- assign :: String -> Expr -> Command
-- assign = Assign

-- wseq :: Command -> Command -> Command
-- wseq = Seq

-- whileExplorer :: WhileExplorer
-- whileExplorer = E.mkExplorer True (==) definterp initialConfig


-- zero = intToExpr 0

-- getRef :: WhileExplorer -> E.Ref
-- getRef = E.currRef


---------------------------------------------------------------
{- Temp for translation,  TODO remove -}
nextCommand :: Command -> Maybe Command
nextCommand Done = Nothing
nextCommand c    = Just c

definterpStepNonDeterm :: Config -> Maybe Command -> [(Maybe Config, Maybe Command)]
definterpStepNonDeterm cfg c =
    case c of
      Nothing -> 
        case cfgEnvironment cfg of
          Environment store threads -> map (threadStep cfg) (permutations threads)
      _ -> 
        case cfgEnvironment cfg of
          Environment store threads -> map (threadStep cfg) (permutations (threads ++ [fromJust c]))
threadStep :: Config -> [Command] -> (Maybe Config, Maybe Command)
threadStep cfg [] = (Just cfg, Nothing)

threadStep cfg threadPermutation =
    
    let newEnv = (cfgEnvironment cfg) { threads = tail threadPermutation }
        newCfg = cfg { cfgEnvironment = newEnv }
    in singleThreadStep newCfg (head threadPermutation)

singleThreadStep :: Config -> Command -> (Maybe Config, Maybe Command)
singleThreadStep cfg c = 
    let ((newCommand, output), newenv) = runState (runWriterT (evalCommand c)) (cfgEnvironment cfg) 
    in 
      case newCommand of
        Done -> (Just cfg { cfgEnvironment = newenv, cfgOutput = cfgOutput cfg ++ output }, (Nothing))
        _ -> (Just cfg { cfgEnvironment = newenv, cfgOutput = cfgOutput cfg ++ output }, (Just newCommand))
