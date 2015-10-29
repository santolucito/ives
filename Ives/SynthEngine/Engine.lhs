> {-# LANGUAGE LambdaCase #-}

> module Ives.SynthEngine.Engine where

> import Ives.SynthEngine.RefinementTypeGen
> import Ives.SynthEngine.TypeExtractor

> import Language.Haskell.Exts
> import qualified Data.Text as T
> import Data.List

the actual synth engine - take a file and generate a program for that satifies exampes given in the "exs" variable

> vroom :: String -> IO()
> vroom f = do
>   fc <- readFile f
>   let typSigs = getTypesFromCode fc
>   case typSigs of
>     Left e -> putStrLn $ show e
>     Right ds -> genHOFxns f (map getName ds) >>= genComponentFxn

once we know which hofxns we are interested in we need to generate the component function.
this should be getting the typeSig of the higherorder functions too
maybe it should also be getting the "exs" example variable
actually dont have the ability to get the code contents yet -means no way to apply functions to the examples
what we CAN do it get all the functions in scope and see which ones will fit the hofxn definition

> genComponentFxn :: [(Name, [(RType, RType)])] -> IO()
> genComponentFxn = pl

> pl = mapM_ (putStrLn . show . fst)
