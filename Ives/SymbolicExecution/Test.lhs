> import Ives.SymbolicExecution.AST

> main = do
>     f1 <- readFile "Sample1.hs"
>     let ast1 = getAST f1
>     putStrLn $ either show show ast1
>     putStrLn "*************"
>     f2 <- readFile "Sample2.hs"
>     let ast2 = getAST f2
>     putStrLn $ either show show ast2
