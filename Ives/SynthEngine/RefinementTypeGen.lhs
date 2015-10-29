> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE ExtendedDefaultRules #-}
> {-# OPTIONS_GHC -fno-warn-type-defaults #-}

> module Ives.SynthEngine.RefinementTypeGen where

> import Prelude 
> import qualified Shelly as S
> import qualified Data.Text as T

> import Language.Haskell.Exts
> import Ives.SynthEngine.TypeExtractor

> import Control.Monad
> import Data.List
> import Data.String.Utils

import ExampleGen

> type RType = String
> data SynthSrc = HigherOrderFxn | Example

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
TODO this seriously needs optimization. 
subtyping will let us prune which rtypes we have to try
we also only need to run step 1 on the higher order fxn identifiers (which we can figure out from TypeExtractor.getTypesFromCode)

> genHOFxns :: FilePath -> [[Name]] -> IO [(Name, [(RType, RType)])]
> genHOFxns f ids = do
>   hofxns <- mapM (foo f) ids
>   ex  <- rTypeAssign Example f "exs"
>   return $ filter (poss ex) hofxns 

TODO: taking head here means we don't support multi-variable type signatures, whatever

> foo :: FilePath -> [Name] -> IO(Name, [(RType,RType)])
> foo f i = do
>   x <- rTypeAssign HigherOrderFxn f (toString $ head i)
>   return (head i, x)

> -- | a hof only fits if one of the rtypes overlaps with the one of the examples rtypes
> poss ex hof = (length $ intersect ex (snd hof)) > 0
> -- | which rtypes does some function in a file satify
> rTypeAssign :: SynthSrc -> FilePath -> String -> IO([(RType,RType)])
> rTypeAssign s file fxn =
>   let
>     f = case s of
>           HigherOrderFxn -> fst
>           Example -> snd
>   in
>     filterM (test file fxn . f) templates

> -- | check if a file (with a single definition) matches the RType using liquidhaskell
> test :: FilePath -> String -> RType -> IO(Bool)
> test f fxnName ty = do
>   fcontents <- readFile f
>   let tmp = f++"tmp.hs"
>   let namedTy = replace "?" fxnName ty
>   writeFile tmp (namedTy ++"\n")
>   appendFile tmp fcontents
>   result <- S.shelly $ S.errExit False $ S.run "liquid" [T.pack tmp]
>   return (isSafe result)

> isSafe :: T.Text -> Bool
> isSafe r = 
>   T.isInfixOf "* SAFE *" r

