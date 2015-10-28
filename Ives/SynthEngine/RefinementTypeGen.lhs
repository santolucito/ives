> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE ExtendedDefaultRules #-}
> {-# OPTIONS_GHC -fno-warn-type-defaults #-}

> module Ives.SynthEngine.RefinementTypeGen where

> import Prelude 
> import qualified Shelly as S
> import qualified Data.Text as T

> import Control.Monad
> import Data.List

import ExampleGen


 main = tester "testLiquid.hs" "examples.hs"

> tester f1 f2 = do
>   hof <- rTypeAssign HigherOrderFxn f1
>   ex  <- rTypeAssign Example f2
>   let possible = intersect hof ex
>   putStrLn "------------------------------"
>   putStrLn "The rtypees that are common to the higher order fxn and the examples are..."
>   putStrLn $ show possible 


> type RType = String
> data SynthSrc = HigherOrderFxn | Example

> templates :: [(RType,RType)]
> templates = 
>   map rTypeTemplate ["=","<=",">="]

we are going to have some trouble with making this more polymorphohc for examples.
the monomorphism restriction in haskell means we can't say (Int,Int) is an instance of (a,b) or even (a,a)
can probably use some rewriting (grab haskell types and put them into rtypes before checking)
Will also need to change '[a]' to 'Measure a =>' 

> -- | (rtype for higherOrderFunction, rtype for list of examples)
> rTypeTemplate op =
>   ("{-@ hofxn :: _ -> i:[a] -> {o:[b] | (len i) "++op ++" (len o)} @-}", "{-@ f :: [([Int],[Int])<{\\i o -> len i "++op++" len o}>] @-}")

> rTypeAssign :: SynthSrc -> FilePath -> IO([(RType,RType)])
> rTypeAssign s exFile =
>   let
>     f = case s of
>           HigherOrderFxn -> fst
>           Example -> snd
>   in
>     filterM (test exFile . f) templates

> -- | check if a file (with a single definition) matches the RType using liquidhaskell
> test :: FilePath -> RType -> IO(Bool)
> test f ty = do
>   fcontents <- readFile f
>   let tmp = f++"tmp.hs"
>   writeFile tmp (ty++"\n")
>   appendFile tmp fcontents
>   result <- S.shelly $ S.errExit False $ S.run "liquid" [T.pack tmp]
>   return (isSafe result)

> isSafe :: T.Text -> Bool
> isSafe r = 
>   T.isInfixOf "* SAFE *" r



