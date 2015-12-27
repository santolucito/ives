> import qualified Ives.SymbolicExecution.SymbolicEngine as Engine
> import qualified Ives.SymbolicExecution.ASTMaker as AST

> main = do
>     putStrLn $ "Stuff compiles :)"
>     src <- readFile "Sample.hs"
>     let res = Engine.initFnSymState $ AST.lookupFn "hue" (AST.getAST src)
>     putStrLn $ show res
