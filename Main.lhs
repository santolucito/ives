
> module Main where

> import Ives.Interface.Haskell.Editor
> import Ives.ExampleGen.Gen
> import Ives.SynthEngine.Engine

> main = do
>   f <- getLine
>   vroom f

 main = vroom "tests/map1.hs"
