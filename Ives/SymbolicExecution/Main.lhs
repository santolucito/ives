> import qualified Ives.SymbolicExecution.SymbolicEngine as Engine
> import qualified Ives.SymbolicExecution.ASTMaker as AST

> main = do
>     putStrLn $ "Stuff compiles :)"
>     src <- readFile "Sample.hs"
>     let ast = AST.getAST src
>     let res = Engine.initFnSymState $ AST.lookupFn "hue" ast
>     putStrLn $ show ast
>     putStrLn $ show res
