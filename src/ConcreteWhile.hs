readme :: IO ()
readme = p $ "This module define a syntactical rewrite to extend while with functions"

data Command = While.Command | Function String [String] Command | FunctionCalll String [Expr]

match :: String -> [String] -> [Expr] -> [Command]
match fname params args = if length params == length args then
    [Assign (fname ++ p) a | (p, a) <- zip params args ]
    else error "number of parameters and arguments dont match"

example = 
    Seq(
    (Function "f" ["arg1", "arg2"] (
        Seq (Print $ Id "arg1")
        ))
    (FunctionCalll "f" [] )
    )

convert = \case of
    FunctionCalll fname args -> 
