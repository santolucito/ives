> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE TupleSections #-}
> {-# LANGUAGE OverloadedStrings #-}

> module Ives.SynthEngine.Engine where

> import Language.Haskell.Exts
> import Language.Haskell.Exts.Pretty
> import qualified Data.Text as T
> import Data.List
> import Data.Char
> import Data.Maybe
> import Data.Either
> import Data.Function
> import Data.Either.Combinators

> import Control.Monad
> import Control.Applicative
> import Control.Arrow
> import qualified Shelly as S

> import Ives.SynthEngine.RefinementTypeGen
> import Ives.SynthEngine.Extractor
> import Ives.SynthEngine.Types

> import Debug.Trace
> import Data.Time.Clock.POSIX

the actual synth engine - take a file and generate a program that satifies examples given in the "exs" variable

> vroom :: String -> IO()
> vroom f = do
>   fc <- readFile f
>
>   startBuildT <- getPOSIXTime
>   (hoTyps, allTyps) <- buildTime fc
>   buildT <- liftM2 (-) getPOSIXTime (return startBuildT)
>
>   putStrLn "starting synth stage"
>   startSynthT <- getPOSIXTime
>   synthTime fc hoTyps allTyps
>   synthT <- liftM2 (-) getPOSIXTime (return startSynthT)
>
>   putStrLn $ "built in "++(show buildT)
>   putStrLn $ "synth in "++(show synthT)

The build time stage only need to happen when the user library is updated.
This builds all the information we need to do pbe based on user defined fxns and imports (e.g. Prelude)
We can be fairly forgiving with runtime here since wouldn't really be a frequent event in the avg use case
With this in mind the "exs" variable shouldn't be used anywhere here, this means
  the weighting function doesn't belong here (since we need rtypes for everything anyway)
  and should be moved to synthTime
NB: a fair amount of time will be added for getting files from disk

> buildTime :: Code -> IO( ([(Sig,[(RType,RType)],Maybe Int)], [Sig]) )
> buildTime fc = do
>   let typSigs = getTypesFromCode fc
>
>   let importSrcs = map showImport $ fromJust $ fromCode fc getImports
>   importSigs <- liftM concat $ mapM getTypesFromModule importSrcs 
> 
>   preludeTypSigs <- getTypesFromModule "base:Prelude"
>
>   let f i tys = map (,Just i) $ filter (isHigherOrder.snd) tys
>
>   let uHOTyps = f 3000 typSigs
>   let iHOTyps = f 2000 importSigs
>   let pHOTyps = f 1000 preludeTypSigs
>   let allHOTyps = uHOTyps ++ iHOTyps ++ pHOTyps :: [(Sig,Maybe Int)]
> --  let allHOTyps = uHOTyps ++ iHOTyps :: [(Sig,Maybe Int)]
> --  let allHOTyps = pHOTyps :: [(Sig,Maybe Int)]
>
> --  mapM print allHOTyps
> --  mapM print typSigs
>   hoRTyps <- mapM (addRType fc) (map fst allHOTyps)
>   print $ length $ filter (\(s,r) -> r/=[]) hoRTyps
>   let hoRTypsW = zipWith (\(t,r) w -> (t,r,w)) hoRTyps (map snd allHOTyps)
>   return (hoRTypsW, typSigs++importSigs++preludeTypSigs)
> --  return (hoRTypsW, typSigs++importSigs)


   let exsTyp = fromJust $ find (\x->"exs"==(toString$fst x)) typSigs

   let weightedU = scoreTyps exsTyp uHOTyps
   let weightedP = scoreTyps exsTyp pHOTyps
   let sortWeightedTyps = reverse. sortWith snd. filter (isJust.snd)
   let p' = (sortWeightedTyps weightedU ++ sortWeightedTyps weightedP)
   putStrLn "-------EXAMPLE OUT TYPE-------"
   (\(l,r) -> f' l >> f' (sndTyp r)) exsTyp
   putStrLn "-------CANDIDATE FXNS-------"
   mapM_ (\(l,r) -> f' l >> f' (lastTyp r)) (uHOTyps ++ pHOTyps)
   putStrLn "-------MATCHED FXNS-------"
   mapM_ (\((l,r),w) -> f' l >> f' (lastTyp r)>> f' w) p'

the Synth time stage happens when the user wants to actaully get a fxn from an example set
this one need to run as quickly as possible
the "exs" should only be read here
synth will need the code file with examples, and all the HOFxns with RTypes

> synthTime :: Code -> [(Sig,[(RType,RType)],Maybe Int)] -> [Sig] -> IO()
> synthTime c hoTyps allTyps = do
>   let exsTyp = fromJust $ find (\t -> "exs" == (toString $ fst t)) allTyps
>   let exsTyMatch = isJust $ uncurry compareTypes $ getExType $ snd exsTyp
>   print exsTyMatch
>   exsRTyp  <- if exsTyMatch 
>               then rTypeAssign Example c (fromJust $ find (\t -> "exs" == (toString $ fst t)) allTyps)
>               else return [noRType]

First we want to rank our higher order functions

>   let hoTyps' = zipWith (\(t,w) r -> (t,r,w)) (scoreTyps exsTyp (map fst3 hoTyps)) (map snd3 hoTyps)

then with the ranks, begin searching for a program

>   let hoFxns = filter (matchRType exsRTyp) (map fstsnd hoTyps')
>   --mapM print hoFxns
>   let candidateFxns = makeFxns (snd exsTyp) (map fst hoFxns) allTyps
> --  mapM print (map fstsnd hoTyps')
>   print candidateFxns
>   validProgs <- applyAll c candidateFxns
>   putStrLn "the following programs satisfy the examples: "
>   mapM_ (putStrLn.("* "++)) validProgs


==================== Move this stuff =================

> scoreTyps :: Sig -> [Sig] -> [(Sig,Maybe Int)]
> scoreTyps exsTyp tys =
>   let
>     f t = (t,isConcreteTypeOf (sndTyp $ snd exsTyp) (lastTyp $ snd t))
>   in
>     map f tys


some nice printers



when we finally have some functions and we want to check if the satisfy the examples

> applyAll :: Code -> [String] -> IO [String]
> applyAll fc fns =
>   let prog fx = fc ++ "\n\nmain = print $ and $ map (\\(i, o) -> ((" ++ fx ++ ") i) == o) exs\n"
>       run fx = do
>           putStrLn $ "testing component: "++fx
>           writeFile "tmp/testCmptFxn.hs" (prog fx)
>           result <- S.shelly $ S.errExit False $ S.run "runhaskell" ["tmp/testCmptFxn.hs"]
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
> buildFxns :: [(Sig,[Sig])] -> [Sig]
> buildFxns cands =
>   let
>     newHOtyp hoTy = foldr1 TyFun $ tail $ tyFunToList hoTy
>     f' (h,cs) = map (\c -> sigCurry h (newHOtyp $ snd h) (toString$fst c) ) cs
>     x = concatMap f' cands
>   in
>     x

> makeFxns :: Type -> [Sig] -> [Sig] -> [String]
> makeFxns exTy hoFxnSig allTyps = 
> -- | take all the code, and the component sig, and get the names of all the fxns that fit component fxn
>   let 
>     sOn hTy n = specializeOn (exAsFunType exTy) hTy
>     hoFxnSig' = map (\s -> (fst s, sOn (snd s) (fst s))) hoFxnSig
>     codePieces = map (\x -> (x,t3 genComponentFxn exTy x allTyps)) hoFxnSig' --a list of compnent fxns for each hofxn
>     hoWithComp = buildFxns codePieces :: [Sig]--with comp fxn applied
>     needsInit t = (length $ tyFunToList t)>2
>     finalTy t = TyFun (fst $lastTyps t) (snd$ lastTyps t)
>     hoWithInit = concatMap (\t -> if trace ((show $fst t) ++ "\n"++
>                                             (show $ tyFunToList $ snd t)++"~~"++
>                                             (show $ needsInit $ snd t)) needsInit $ snd t  
>                                   then trace ((show$finalTy$snd t)++" "
>                                               ++(show t)++" --"++(show $coerceSig (finalTy$ snd t) t)) coerceSig (finalTy$ snd t) t
>                                   else [t]) hoWithComp
>   in
>     map (toString.fst) $ hoWithInit


take a higher order function that still needs some initial values to be ready for application to examples

> genInitValues :: Sig -> [Sig]
> genInitValues ty = 
>   let
>     (inTy,outTy) = lastTyps $ snd ty
>     target = foldr1 TyFun [inTy,outTy]
>     funList = tail $ tyFunToList $ snd ty 
>     cur = (fst ty, if length funList ==0 then TyVar (Ident ":(") else foldr1 TyFun funList)
>     newFxns' = trace (show target ++ "\n"++show cur) $ coerceSig target cur
>     newFxns  = mapSnd (TyFun $ head $ rights $ [getComp (snd ty)]) newFxns'
>   in 
>     newFxns

Given a higher order function, we want all compenent functions that fit that type sig
The HO typ sig can generalize the example type, but so must the component sig 
we cant have map :: (a->b)->[a]->[b] with exs::[Int]->[Int] and expect f::[Bool]->[Bool]

> sigCurry :: Sig -> Type -> String -> Sig
> sigCurry (name, _) ty x = (Ident newName, ty)
>   where newName = toString name ++ " (" ++ x ++ ")"


> -- | turn a current sig in to a target, by applying all different combos of init vals
> coerceSig :: Type -> Sig -> [Sig]
> coerceSig target cur =
>     if isJust (isConcreteTypeOf (snd cur) target) ||
>        isJust (isConcreteTypeOf target (snd cur))
>     then [cur]
>     else case snd cur of
>            TyFun (TyCon (UnQual (Ident "Int"))) fn -> dfl cur fn ["-2", "-1", "0", "1", "2", "3"]
>            TyFun (TyCon (UnQual (Ident "Integer"))) fn -> dfl cur fn ["-2", "-1", "0", "1", "2"]
>            TyFun (TyCon (UnQual (Ident "Double"))) fn -> dfl cur fn ["0.0", "1.0"]
>            TyFun (TyList _) fn -> dfl cur fn ["[]"]
>            -- Instance of Monoid ->  dfl cur fn ["mempty"]
>            otherwise ->  []
>   where dfl cur fn = concatMap (coerceSig target . sigCurry cur fn)

> genComponentFxn :: Type -> Sig -> [Sig] -> [Sig]
> genComponentFxn exTy hofxnSig allTyps = 
>  let
>    componentSig = getComp $ snd hofxnSig :: Either String Type
>    progs = case componentSig of
>              Left e -> trace ("genComponentFxn: " ++ e ++ "\n") []
>              Right cs -> concatMap (\t -> t2 coerceSig cs t) allTyps
>  in
>    progs

> --f' :: Pretty a => a -> IO()
> -- f' = putStrLn . prettyPrint

> t1 p1 = trace (show p1) p1

> t2 f p1 p2 =
>   let 
>     s1 = prettyPrint p1
>     s2 = (toString $fst p2) ++" :: "++ (prettyPrint $snd p2)
>    -- s2 = prettyPrint p2
>    in trace ("Testing with\n"++s1++",\n"++s2++" =\n"++
>             (show $ f p1 p2)++"\n\n") (f p1 p2)

> t3 f p1 p2 p3 =
>   let 
>     s1 = prettyPrint p1
>     s2 = (toString $fst p2) ++ (prettyPrint $snd p2)
>     s3 = ""
>    in trace ("Testing with\n"++s1++",\n"++s2++","++s3++" =\n"++
>             (show $ f p1 p2 p3)++"\n\n") (f p1 p2 p3)

> sortWith f = sortBy (compare `on` f)

> fst3 (x,_,_) = x
> snd3 (_,x,_) = x
> trd3 (_,_,x) = x
> fstsnd (x,y,_) = (x,y)
> mapSnd f = map (\(x,y) -> (x,f y))
> mapFst f = map (\(x,y) -> (f x,y))









