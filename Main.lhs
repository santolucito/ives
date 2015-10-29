> {-# LANGUAGE LambdaCase #-}

> module Main where

> import Ives.Interface.Haskell.Editor
> import Ives.ExampleGen.Gen
> import Ives.SynthEngine.RefinementTypeGen
> import Ives.SynthEngine.Engine

> import Language.Haskell.Exts.Parser
> import Language.Haskell.Exts

> import qualified Data.Text as T
> import Data.List

 main = haskEditor exGen

> main = do
>  testFile "tests/userApi.hs"
>  rTypeTester "tests/userApi.hs"

   putStrLn "------------------------------"
   putStrLn "------------------------------"
   putStrLn "and the types of the fxns that we have 'in scope'"
   testFile "tests/test.hs"

> testFile f = do
>   fc <- readFile f
>   let x = processTys (T.pack fc)
>   case x of
>     Left e -> putStrLn $ show e
>     Right ds -> pl $ map getName ds

> pl = mapM_ (putStrLn . show)

> rTypeTester f = do
>   hof1 <- rTypeAssign HigherOrderFxn f "duplIf"
>   hof2 <- rTypeAssign HigherOrderFxn f "takeWhilePlus"
>   putStrLn "test"
>   ex  <- rTypeAssign Example f "exs"
>   let possible1 = (length $ intersect hof1 ex) > 0
>   let possible2 = (length $ intersect hof2 ex) > 0
>   putStrLn $ show possible1
>   putStrLn $ show possible2
