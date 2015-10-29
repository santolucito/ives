> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE ExtendedDefaultRules #-}
> {-# OPTIONS_GHC -fno-warn-type-defaults #-}

> module Ives.SynthEngine.RefinementTypeGen where

> import Prelude 
> import qualified Shelly as S
> import qualified Data.Text as T

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

