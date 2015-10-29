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


> main :: IO()
> main = do
>   fc <- readFile "tests/userApi.hs"
>   let ids = processTys (T.pack fc)
>   case ids of
>     Left e -> putStrLn $ show e
>     Right ds -> rTypeTester "tests/userApi.hs" (map getName ds)

> foo :: FilePath -> [Name] -> IO(Name, [(RType,RType)])
> foo f i = do
>   x <- rTypeAssign HigherOrderFxn f (toString $ head i)
>   return (head i, x)

> poss ex hof = (length $ intersect ex (snd hof)) > 0

> rTypeTester :: FilePath -> [[Name]] -> IO()
> rTypeTester f ids = do
>   hofxns <- mapM (foo f) ids
>   ex  <- rTypeAssign Example f "exs"
>   let possibleHofxns = filter (poss ex) hofxns 
>   pl possibleHofxns
> pl = mapM_ (putStrLn . show . fst)

> toString :: Name -> String
> toString = \case
>   Ident s -> s
>   Symbol s -> s 

