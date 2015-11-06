> {-# LANGUAGE LambdaCase #-}

> module Ives.SynthEngine.Engine where

> import Language.Haskell.Exts
> import qualified Data.Text as T
> import Data.List
> import Data.Either.Combinators
> import Control.Monad

> import Ives.SynthEngine.RefinementTypeGen
> import Ives.SynthEngine.Extractor
> import Ives.SynthEngine.Types


the actual synth engine - take a file and generate a program that satifies examples given in the "exs" variable

> vroom :: String -> IO()
> vroom f = do
>   fc <- readFile f
>   let typSigs = getTypesFromCode fc
>   let exs = undefined --getExamples fc
>   either putStrLn (proceed exs fc) typSigs

> proceed exs fc ts = do
>   hoFxns <- genHOFxns fc ts
>   let candidateFxns = map (\x -> (fst x,genComponentFxn fc x)) hoFxns --a list of compnent fxns for each hofxn
>   let validProgs = applyAll exs candidateFxns
>   putStrLn "the following programs satisfy the examples"
>   mapM_ putStrLn validProgs

once we know which hofxns we are interested in we need to generate the component function.
this should be getting the typeSig of the higherorder functions too
maybe it should also be getting the "exs" example variable
actually dont have the ability to get the code contents yet -means no way to apply functions to the examples
what we CAN do it get all the functions in scope and see which ones will fit the hofxn definition

each hofxn has a number of possible component fxns
we need to compose these functions and run them on the examples until we find on that works.

> -- | examples -> [hofxns,[componentFxns]] -> [validPrograms]
> applyAll :: [[a]] -> [((Name,Type),[(Name,Type)])] -> [String]
> applyAll exs cands =
>   let
>     f = map (toString.fst) 
>     (hs,cs) = unzip cands
>     csNames = zip (f hs) (map f cs) :: [(String,[String])]
>     f' (h,c) = (map . (++)) h c
>     x = map f' csNames
>   in
>     intercalate " " x

     ["hofxns"]++map show hs ++ ["componentFxns"]++map show cs

> -- | take all the code, and the component sig, and get the names of all the fxns that fit component fxn
> genComponentFxn :: Code -> ((Name,Type), [(RType, RType)]) -> [(Name,Type)]
> genComponentFxn c hofxnSig = 
>  let
>    componentSig = getComp $ snd $ fst hofxnSig :: Either String Type
>    x = case componentSig of
>        Left e -> Left e
>        Right s -> mapRight (filter (\x -> s == snd x)) $ getTypesFromCode c :: Either String [(Name,Type)]
>    in 
>      fromRight' x

