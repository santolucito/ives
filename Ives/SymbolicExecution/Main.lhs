> import qualified Language.Haskell.Exts as Exts
> import qualified Ives.SymbolicExecution.SymbolicEngine as Eng
> import qualified Ives.SymbolicExecution.AST as AST

> main = do
>     src <- readFile "Sample.hs"
>     let ast = AST.getAST src
>     let res = Eng.execute "hue" ast
>     putStrLn "AST:"
>     putStrLn $ show ast
>     putStrLn ""
>     putStrLn "PathCons:"
>     putStrLn $ show res
