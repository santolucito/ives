> import Ives.SymbolicExecution.AST

> main = do
>     f <- readFile "Sample1.hs"
>     let ast = getAST f
>     putStrLn $ either show show ast
