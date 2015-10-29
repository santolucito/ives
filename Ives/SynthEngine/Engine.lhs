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
>   let ids = getTypesFromCode fc
>   case ids of
>     Left e -> putStrLn $ show e
>     Right ds -> rTypeTester f (map getName ds)

1) try to assign *every* rtype to *every* identifier definied in the file
2) try to assign *every* rtype to the examples
3) see which identifiers have an rtype that cooresponds to on of the rtypes that the examples satisfy
TODO this seriously needs optimization. 
subtyping will let us prune which rtypes we have to try
we also only need to run step 1 on the higher order fxn identifiers (which we can figure out from TypeExtractor.getTypesFromCode)

> rTypeTester :: FilePath -> [[Name]] -> IO()
> rTypeTester f ids = do
>   hofxns <- mapM (foo f) ids
>   ex  <- rTypeAssign Example f "exs"
>   let possibleHofxns = filter (poss ex) hofxns 
>   pl possibleHofxns

TODO: taking head here means we don't support multi-variable type signatures, whatever

> foo :: FilePath -> [Name] -> IO(Name, [(RType,RType)])
> foo f i = do
>   x <- rTypeAssign HigherOrderFxn f (toString $ head i)
>   return (head i, x)

> -- | a hof only fits if one of the rtypes overlaps with the one of the examples rtypes
> poss ex hof = (length $ intersect ex (snd hof)) > 0

> pl = mapM_ (putStrLn . show . fst)


