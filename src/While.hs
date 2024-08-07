module While where

import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Control.Monad.Trans.Writer.Lazy
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import Control.Monad.Identity

import Control.Monad.Reader

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

-- emptyConfig :: Config
-- emptyConfig = Config {current cfgStore = Map.empty, cfgOutput = [], cfgThreads = [] }

type Store = Map.Map String Literal
type StoreM = State Store

type Output = [String]

type Thread = (Integer, Command)
type Threads = [Thread]

data Config = Config { current::Thread, cfgStore :: Store, cfgOutput :: Output, cfgThreads :: Threads} deriving (Show, Eq)

evalPlus :: Expr -> Expr -> Reader Store Expr
evalPlus (LitExpr (LitInt l1)) (LitExpr (LitInt l2)) = return $ LitExpr $ LitInt (l1 + l2)
evalPlus (LitExpr l1) l2 = do
    l2' <- evalExpr l2
    return (Plus (LitExpr l1) l2')
evalPlus l1 l2 = do
    l1' <- evalExpr l1
    return (Plus l1' l2)


evalLeq :: Expr -> Expr -> Reader Store Expr
evalLeq (LitExpr (LitInt l1)) (LitExpr (LitInt l2)) = return $ LitExpr $ LitBool (l1 <= l2)
evalLeq (LitExpr l1) e2 = do
    e2' <- evalExpr e2
    return (Leq (LitExpr l1) e2')
evalLeq e1 e2 = do
    e1' <- evalExpr e1
    return (Leq e1' e2)

evalEq :: Expr -> Expr -> Reader Store Expr
evalEq (LitExpr (LitInt l1)) (LitExpr (LitInt l2)) = return $ LitExpr $ LitBool (l1 == l2)
evalEq (LitExpr l1) e2 = do
    e2' <- evalExpr e2
    return (Eq (LitExpr l1) e2')
evalEq e1 e2 = do
    e1' <- evalExpr e1
    return (Eq e1' e2)

evalAnd :: Expr -> Expr -> Reader Store Expr
evalAnd e@(LitExpr (LitBool False)) _ = return e

evalAnd (LitExpr (LitBool b1)) (LitExpr (LitBool b2)) = return $ LitExpr $ LitBool (b1 && b2)
evalAnd (LitExpr l1) e2 = do
    e2' <- evalExpr e2
    return (And (LitExpr l1) e2')
evalAnd e1 e2 = do
    e1' <- evalExpr e1
    return (And e1' e2)

evalExpr :: Expr -> Reader Store Expr
evalExpr (LitExpr e) = return $ LitExpr e
evalExpr (Plus e1 e2) = evalPlus e1 e2
evalExpr (Leq e1 e2) = evalLeq e1 e2
evalExpr (Eq e1 e2) = evalEq e1 e2

evalExpr (And e1 e2) = evalAnd e1 e2
evalExpr (Id s) = do
    store <- ask
    let l = Map.lookup s store
    case l of
        Just lit -> return $ LitExpr lit
        Nothing -> error $ "Invalid Id: " ++ s

evalExpr' :: Expr -> Reader Store Expr
evalExpr' (LitExpr e) = return $ LitExpr e
evalExpr' e = do
    e' <- evalExpr e
    evalExpr' e'


evalThread :: Command -> WriterT Output (State (Store, Threads)) Command
evalThread c = do
    (store, threads) <- lift get
    lift $ put (store, ( 1 +(max_id threads), c) : threads)
    return Done 
    where 
        max_id [] = 0
        max_id threads = maximum $ map fst threads
evalCommand :: Command -> WriterT Output (State (Store, Threads)) Command
evalCommand (Print e) = do
    store <- lift $ state $ \(s, t) -> (s, (s, t))
    x <- lift $ lift $ runReaderT (evalExpr' e) store
    tell [show x]
    return Done

evalCommand Done = return Done

evalCommand (Assign id e) = do
    (store, threads) <- lift get
    ex <- lift . lift $ runReaderT (evalExpr' e) store
    case ex of
        LitExpr lit -> do 
            lift $ put (Map.insert id lit store, threads)
            return Done
        _ -> error "Expression did not evaluate to a literal."


evalCommand (Seq Done c2) = return c2
evalCommand (Seq c1 c2) = do
    c1' <- evalCommand c1
    return $ Seq c1' c2

evalCommand (While (LitExpr (LitBool False)) e2 c) = return Done
evalCommand (While (LitExpr (LitBool True)) e2 c) = return $ Seq c (While e2 e2 c)
evalCommand (While e1 e2 c) = do
    store <- lift $ state $ \(s, t) -> (s, (s, t))
    e1' <- lift $ lift $ runReaderT (evalExpr' e1) store
    return $ While e1' e2 c

evalCommand (Thread c) = evalThread c 


evalCommand' :: Command -> WriterT Output (State (Store, Threads)) Command
evalCommand' Done = return Done
evalCommand' c = do
    c' <- evalCommand c
    evalCommand' c'

step :: Config -> [Config]

step (Config (t@(i1, c1)) s1 a1 t1) =
    map (\ti@(i, ci) -> 
        let ((ci', a2), (s2, t2)) = runState (runWriterT (evalCommand ci)) (s1, delete ti (t : t1))
        in ( Config {current = (i, ci'), cfgStore = s2, cfgOutput = a1 ++ a2, cfgThreads = t2})
         ) (not_done (t : t1))
    where 
        not_done thread_pool = filter ((/= Done) . snd) thread_pool


done :: (Config) -> Bool
done (Config (_, Done) _ _ _) = True
done (Config (_, Done) _ _ t) = and $ map ((==) Done . snd) t
done _ = False

allDone = and . map done

run' :: [Config] -> [Config]
run' l = 
    let r = concatMap step l
    in if allDone r then r
        else run' r

wrapProgram :: Command -> Config
wrapProgram c= Config {current = (0, c), cfgStore = Map.empty, cfgOutput = [], cfgThreads = [] }


-- Run a program with all possible interleavings
run :: Command -> [Output]
run c = toOutput $ run' [wrapProgram c]
    where 
        toOutput :: [Config] -> [Output]
        toOutput = map toOutput'

        toOutput' :: (Config) -> Output
        toOutput' (Config _ _ a _) = a
