> module Ives.SynthEngine.Engine where

> import Language.Haskell.GhcMod

this will grab all the defintions (fxns, types, etc) from Prelude and spit them back as a string. Do I just parse this fucking string to build a grammar of types. I want types of types. I really need dependent types i think... ruzica will kill me if i suggest Agda

> b :: IO ()
> b = do
>   r <- runGhcModT (defaultOptions {detailed = True}) $ browse "base:Prelude"
>   putStrLn $ show r

