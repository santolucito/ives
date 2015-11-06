> import Ives.SymbolicExecution.FnEnv

> main = do
>     f1 <- readFile "Sample.hs"
>     let env = fnEnv f1
>     putStrLn $ show env
>     putStrLn "**************"
>     putStrLn $ show (lookup "fib" env)
