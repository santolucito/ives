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
>   (fc,typs) <- buildTime f
> --  synthTime fc (fromRight [] typSigs++fromRight [] preludeTypSigs)
>   return ()

> buildTime :: String -> IO(Code,[(Name,Type)])
> buildTime f = do
>   fc <- readFile f
>   let typSigs' = getTypesFromCode fc
>   whenLeft typSigs' putStrLn
>   let typSigs = fromRight [] typSigs'
>
>   preludeTypSigs <- getTypesFromModule "base:Prelude"
>   whenLeft preludeTypSigs putStrLn
>
>   let exsTyp = fromJust $ find (\x->"exs"==(toString$fst x)) typSigs
>
>   let uHOTyps = filter isHigherOrder typSigs
>   let pHOTyps = filter isHigherOrder (fromRight [] preludeTypSigs)
>   let weightedU = scoreTyps exsTyp uHOTyps
>   let weightedP = scoreTyps exsTyp pHOTyps
>   let sortWeightedTyps = reverse. sortWith snd. filter (isJust.snd)
>   let p' = (sortWeightedTyps weightedU ++ sortWeightedTyps weightedP)
>   putStrLn "-------EXAMPLE OUT TYPE-------"
>   (\(l,r) -> f' l >> f' (sndTyp r)) exsTyp
>   putStrLn "-------CANDIDATE FXNS-------"
>   mapM_ (\(l,r) -> f' l >> f' (lastTyp r)) (uHOTyps ++ pHOTyps)
>   putStrLn "-------MATCHED FXNS-------"
>   mapM_ (\((l,r),w) -> f' l >> f' (lastTyp r)>> f' w) p'
> --  let exs = undefined --getExamples fc
>   return (fc,(map fst p'))

> synthTime fc typs = do
>   let hoSigs = filter isHigherOrder typs
>   hoFxns <- genHOFxns fc hoSigs
>   let candidateFxns = makeFxns fc hoFxns
>   validProgs <- applyAll fc candidateFxns
>   putStrLn "the following programs satisfy the examples: "
>   mapM_ (putStrLn.("* "++)) validProgs


==================== Move this stuff =================

> scoreTyps :: (Name,Type) -> [(Name,Type)] -> [((Name,Type),Maybe Int)]
> scoreTyps exsTyp tys =
>   let
>     f t = (t,compareExTypeToHOType (sndTyp $ snd exsTyp) (lastTyp $ snd t))
>   in
>     map f tys


some nice printers

> --f' :: Pretty a => a -> IO()
> -- f' = putStrLn . prettyPrint
> f' :: Show a => a -> IO()
> f' = putStrLn . show


when we finally have some functions and we want to check if the satisfy the examples

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

> sortWith f = sortBy (\x y -> compare (f x) (f y))

