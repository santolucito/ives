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

 liquidTypeInjected.hs:1:5: Error: Specified Type Does Not Refine Haskell Type for Main.exs
 Haskell: [([GHC.Types.Bool], [GHC.Types.Bool])]
 Liquid : forall a. [([a], [a])]

we assume ?type? is already wrapped in a list

would like to use a nicer example notation,but liquid haskell doesnt support language exts
could write a small function to rewrite :-> as a tuple, but that requires a bit of effort
{-# LANGUAGE TypeOperators #-}
data a :-> b =  a :-> b
fst :: (a :-> b) -> a
fst (a :-> b) = a

> -- | (rtype for higherOrderFunction, rtype for list of examples)
> rTypeTemplate op =
>   ("{-@ ?f? :: _ -> i:[a] -> {o:[a] | (len i) "++op ++" (len o)} @-}", "{-@ ?f? :: [(?type?,?type?)<{\\i o -> len i "++op++" len o}>] @-}")


------ Algorithm ----------
1) try to assign *every* rtype to *every* identifier definied in the file
2) try to assign *every* rtype to the examples
3) see which identifiers have an rtype that cooresponds to on of the rtypes that the examples satisfy
TODO this _seriously_ needs optimization. 
subtyping will let us prune which rtypes we have to try
we also only need to run step 1 on the higher order fxn identifiers (which we can figure out from TypeExtractor.getTypesFromCode)

> genHORTyps :: Code -> [(Name,Type)] -> IO [((Name,Type), [(RType, RType)])]
> genHORTyps c sigs = 
>   mapM (addRType c) sigs


> addRType :: Code -> (Name,Type) -> IO((Name,Type), [(RType,RType)])
> addRType c t = do
>   x <- rTypeAssign HigherOrderFxn c t 
>   return (t, x)


   
> -- | a hof only fits if one of the rtypes overlaps with the one of the examples rtypes
> poss ex hof = (length $ intersect ex (snd hof)) > 0

> -- | which rtypes does some function in a file satify
> rTypeAssign :: SynthSrc -> Code -> (Name,Type) -> IO([(RType,RType)])
> rTypeAssign s c fxn =
>   let
>     f = case s of
>           HigherOrderFxn -> fst
>           Example -> snd
>   in
>     filterM (test c fxn . f) templates

> -- | check if a file (with a single definition) matches the RType using liquidhaskell
> test :: Code -> (Name,Type) -> RType -> IO(Bool)
> test fc fxn ty = do
>   let fxnName = toString $ fst fxn
>   putStrLn $ "testing RType for: " ++ fxnName
>   let tmp = "tmp/liquidTypeInjected.hs"
>   let ty' = replace "?f?" fxnName ty
>   let namedTy = replace "?type?" (prettyPrint $ getOutExType $ snd fxn) ty' --need some way to write down a type (haskell-src pretty printer?)
>   writeFile tmp (namedTy ++"\n")
>   appendFile tmp fc
>   S.shelly $ S.errExit False $ S.bash_ "liquid" [T.pack tmp,">tmp/liquid.results 2>&1"]
>   liftM isSafe (S.shelly $ S.readfile "tmp/liquid.results")

> isSafe :: T.Text -> Bool
> isSafe r = 
>   T.isInfixOf "* SAFE *" r

