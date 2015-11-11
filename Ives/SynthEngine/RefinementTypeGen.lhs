> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE ExtendedDefaultRules #-}
> {-# OPTIONS_GHC -fno-warn-type-defaults #-}

> module Ives.SynthEngine.RefinementTypeGen where

> import Prelude 
> import qualified Shelly as S
> import qualified Data.Text as T
> import Control.Monad
> import Data.List
> import Data.Either.Combinators
> import Data.String.Utils
> import Language.Haskell.Exts

> import Ives.SynthEngine.Types
> import Ives.SynthEngine.Extractor


> templates :: [(RType,RType)]
> templates = 
>   map rTypeTemplate ["=","<=",">="]

we are going to have some trouble with making this more polymorphohc for examples.
we can't say (Int,Int) is an instance of (a,b) or even (a,a)
the monomorphism restriction in haskell also means the user must provide a signature or we could get something odd
can probably use some rewriting (grab haskell types and put them into rtypes before checking)
Will also need to change '[a]' to 'Measure a =>' 

> -- | (rtype for higherOrderFunction, rtype for list of examples)
> rTypeTemplate op =
>   ("{-@ ? :: _ -> i:[a] -> {o:[a] | (len i) "++op ++" (len o)} @-}", "{-@ ? :: [([Int],[Int])<{\\i o -> len i "++op++" len o}>] @-}")


------ Algorithm ----------
1) try to assign *every* rtype to *every* identifier definied in the file
2) try to assign *every* rtype to the examples
3) see which identifiers have an rtype that cooresponds to on of the rtypes that the examples satisfy
TODO this _seriously_ needs optimization. 
subtyping will let us prune which rtypes we have to try
we also only need to run step 1 on the higher order fxn identifiers (which we can figure out from TypeExtractor.getTypesFromCode)

> genHOFxns :: Code -> [(Name,Type)] -> IO [((Name,Type), [(RType, RType)])]
> genHOFxns c sigs = do
>   hofxns <- mapM (addRType c) sigs
>   ex  <- rTypeAssign Example c "exs"
>   return $ filter (poss ex) hofxns 

> genHOFxns' :: Code -> [(Name,Type)] -> IO [((Name,Type), [(RType, RType)])]
> genHOFxns' c sigs = do
>   hofxns <- mapM (addRType c) sigs
>   ex  <- rTypeAssign Example c "exs"
>   return $ filter (poss ex) hofxns 

> addRType :: Code -> (Name,Type) -> IO((Name,Type), [(RType,RType)])
> addRType c t = do
>   let n = toString $ fst t
>   x <- rTypeAssign HigherOrderFxn c n 
>   return (t, x)


   
> -- | a hof only fits if one of the rtypes overlaps with the one of the examples rtypes
> poss ex hof = (length $ intersect ex (snd hof)) > 0

> -- | which rtypes does some function in a file satify
> rTypeAssign :: SynthSrc -> Code -> String -> IO([(RType,RType)])
> rTypeAssign s c fxn =
>   let
>     f = case s of
>           HigherOrderFxn -> fst
>           Example -> snd
>   in
>     filterM (test c fxn . f) templates

> -- | check if a file (with a single definition) matches the RType using liquidhaskell
> test :: Code -> String -> RType -> IO(Bool)
> test fc fxnName ty = do
>   putStrLn $ "testing RType for: " ++ fxnName
>   let tmp = "tmp/liquidTypeInjected.hs"
>   let namedTy = replace "?" fxnName ty
>   writeFile tmp (namedTy ++"\n")
>   appendFile tmp fc
>   S.shelly $ S.errExit False $ S.bash_ "liquid" [T.pack tmp,">tmp/liquid.results 2>&1"]
>   liftM isSafe (S.shelly $ S.readfile "tmp/liquid.results")

> isSafe :: T.Text -> Bool
> isSafe r = 
>   T.isInfixOf "* SAFE *" r

