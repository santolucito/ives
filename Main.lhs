> module Main where

> import System.Environment
>-- import Ives.Interface.Haskell.Editor
>-- import Ives.ExampleGen.Gen
> import Ives.SynthEngine.Engine
> import Ives.SynthEngine.Extractor

> main = do
>   args <- getArgs
>   mapM_ vroom args

