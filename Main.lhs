
> module Main where

> import Ives.Interface.Haskell.Editor
> import Ives.ExampleGen.Gen
> import Ives.SynthEngine.Engine
> import Ives.SynthEngine.Extractor


> main = do
>   putStrLn "give me a file"


   getTypesFromModule "Euterpea:Euterpea.Music.Note.Music" >>= print
   getTypesFromModule "Data.List" >>= print

>   f <- getLine
>   vroom f

 main = vroom "tests/map1.hs"
