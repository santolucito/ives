> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE OverloadedStrings #-}

> module Ives.SynthEngine.Engine where

> import Language.Haskell.Exts
> import Language.Haskell.Exts.Pretty
> import qualified Data.Text as T
> import Data.List
> import Data.Char
> import Data.Maybe
> import Data.Either.Combinators
> import Control.Monad
> import Control.Arrow
> import qualified Shelly as S

> import Ives.SynthEngine.RefinementTypeGen
> import Ives.SynthEngine.Extractor
> import Ives.SynthEngine.Types

the actual synth engine - take a file and generate a program that satifies examples given in the "exs" variable

> vroom :: String -> IO()
> vroom f = do
>   fc <- readFile f
>   let typSigs' = getTypesFromCode fc
>   whenLeft typSigs' putStrLn
>   let typSigs = fromRight [] typSigs'
>
>   preludeTypSigs <- getTypesFromModule "base:Prelude"
>   whenLeft preludeTypSigs putStrLn
>
>   let exsTyp = fromJust $ find (\x->"exs"==(toString$fst x)) typSigs
>   let p = filter isHigherOrder (fromRight [] preludeTypSigs)
>   let p' = filter (\t -> isHigherOrder t && compareTypes (snd exsTyp) (snd t)) p
>   (\(l,r) -> f' l >> f' r) exsTyp
>   putStrLn "--------------"
>   mapM_ (\(l,r) -> f' l >> f' r) p'
>   putStrLn "--------------"
>   mapM_ (\(l,r) -> f' l >> f' r) p
> --  let exs = undefined --getExamples fc
> --  proceed exs fc (fromRight [] typSigs++fromRight [] preludeTypSigs)

> f' :: Pretty a => a -> IO()
> f' = putStrLn . prettyPrint

> proceed exs fc typs = do
>   let hoSigs = filter isHigherOrder typs
>   hoFxns <- genHOFxns fc hoSigs
>   let candidateFxns = makeFxns fc hoFxns
>   validProgs <- applyAll fc candidateFxns
>   putStrLn "the following programs satisfy the examples: "
>   mapM_ (putStrLn.("* "++)) validProgs

> applyAll :: Code -> [String] -> IO [String]
> applyAll fc fns =
>   let prog fx = fc ++ "\n\nmain = print $ and $ map (\\(i, o) -> ((" ++ fx ++ ") i) == o) exs\n"
>       run fx = do
>           writeFile "tmp0000.hs" (prog fx)
>           result <- S.shelly $ S.errExit False $ S.run "runhaskell" ["tmp0000.hs"]
>           return ((T.filter isAlpha result) == "True")
>   in filterM run fns
>      


once we know which hofxns we are interested in we need to generate the component function.
this should be getting the typeSig of the higherorder functions too
maybe it should also be getting the "exs" example variable
actually dont have the ability to get the code contents yet -means no way to apply functions to the examples
what we CAN do it get all the functions in scope and see which ones will fit the hofxn definition

each hofxn has a number of possible component fxns
we need to compose these functions and run them on the examples until we find on that works.

> -- | [hofxns,[componentFxns]] -> [Programs]
> buildFxns :: [((Name,Type),[(Name,Type)])] -> [String]
> buildFxns cands =
>   let
>     f = map (toString.fst) 
>     (hs,cs) = unzip cands
>     csNames = zip (f hs) (map f cs) :: [(String,[String])]
>     f' (h,c) = map ((h ++ " ")++) c
>     x = map f' csNames
>   in
>     concat x

     ["hofxns"]++map show hs ++ ["componentFxns"]++map show cs

> makeFxns :: Code -> [((Name,Type), [(RType, RType)])] -> [String]
> makeFxns c hoFxnSig = 
> -- | take all the code, and the component sig, and get the names of all the fxns that fit component fxn
>   let 
>     codePieces = map (\x -> (fst x,genComponentFxn c x)) hoFxnSig --a list of compnent fxns for each hofxn
>   in
>     buildFxns codePieces

> genComponentFxn :: Code -> ((Name,Type), [(RType, RType)]) -> [(Name,Type)]
> genComponentFxn c hofxnSig = 
>  let
>    componentSig = getComp $ snd $ fst hofxnSig :: Either String Type
>    x = case componentSig of
>        Left e -> Left e
>        Right s -> mapRight (filter (\x -> s == snd x)) $ getTypesFromCode c :: Either String [(Name,Type)]
>  in
>    fromRight' x
