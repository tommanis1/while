module Test where
import While
import qualified Data.Map as Map

p = Thread (Seq( Print . LitExpr . LitString $ "0") (Print . LitExpr . LitString $ "1"))

cc = Config {cfgStore = Map.empty, cfgOutput = [], cfgThreads = [Seq( Print . LitExpr . LitString $ "0") (Print . LitExpr . LitString $ "1")]}

p0 = Seq (Thread (Seq( Print . LitExpr . LitString $ "0") (Print . LitExpr . LitString $ "1"))) (Thread (Seq( Print . LitExpr . LitString $ "2") (Print . LitExpr . LitString $ "3")))

main = putStrLn $ show $ run p0
-- p =
--     Thread (
--         Seq( (Assign "i1" (LitExpr $ LitInt 0))

--         )
--     )
