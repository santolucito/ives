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
> import qualified Data.Map.Strict as Map

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
>   let hoRTypsW = zipWith (\(t,r) w -> (t,r,w)) hoRTyps (map snd allHOTyps)
>   return (hoRTypsW, typSigs++importSigs++preludeTypSigs)
>  -- return (hoRTypsW, typSigs++importSigs)


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
> --  validProgs <- applyAll c candidateFxns
>   putStrLn "the following programs satisfy the examples: "
> --  mapM_ (putStrLn.("* "++)) validProgs


==================== Move this stuff =================

> scoreTyps :: Sig -> [Sig] -> [(Sig,Maybe Int)]
> scoreTyps exsTyp tys =
>   let
>     f t = (t,isConcreteTypeOf (sndTyp $ snd exsTyp) (lastTyp $ snd t))
>   in
>     map f tys


some nice printers

> --f' :: Pretty a => a -> IO()
> -- f' = putStrLn . prettyPrint


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
>     f' (h,cs) = map (\c -> exCurry h (newHOtyp $ snd h) (toString$fst c) ) cs
>     x = concatMap f' cands
>   in
>     x

> makeFxns :: Type -> [Sig] -> [Sig] -> [String]
> makeFxns exTy hoFxnSig allTyps = 
> -- | take all the code, and the component sig, and get the names of all the fxns that fit component fxn
>   let 
>     sOn hTy n = specializeOn (exAsFunType exTy) hTy
>     hoFxnSig' = map (\s -> (fst s, sOn (snd s) (fst s))) hoFxnSig
>     codePieces = map (\x -> (x,genComponentFxn exTy x allTyps)) hoFxnSig' --a list of compnent fxns for each hofxn
>     hoWithComp = trace (show codePieces) buildFxns codePieces :: [Sig]--with comp fxn applied
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

> -- | instantiate the tyVars in abstTy with the TyCons in concTy
> --   we assume that the concTy is the ex fxn for the abstTy (a higher order fxn)
> specializeOn :: Type -> Type -> Type
> specializeOn concTy abstTy = 
>   let
>     abstTyFxn = lastAsFunType abstTy
>     funMap = makeFunMap concTy (lastAsFunType abstTy) Map.empty
>   in
>     replaceTysIn abstTy funMap

> replaceTysIn :: Type -> Map.Map Type Type -> Type
> replaceTysIn (TyVar n) m = fromMaybe (TyVar n)  (Map.lookup (TyVar n) m)
> replaceTysIn (TyCon n) m = TyCon n 
> replaceTysIn (TyApp t1 t2) m = TyApp (replaceTysIn t1 m) (replaceTysIn t2 m)
> replaceTysIn (TyFun t1 t2) m = TyFun (replaceTysIn t1 m) (replaceTysIn t2 m)
> replaceTysIn (TyList t) m = TyList (replaceTysIn t m)
> replaceTysIn (TyParen t) m = TyParen (replaceTysIn t m)
> replaceTysIn ty m = ty --add tuple case!

> makeFunMap :: Type -> Type -> Map.Map Type Type -> Map.Map Type Type
> makeFunMap (TyCon _) (TyCon _) m = m --its not gonna work out between us
> makeFunMap (TyCon n) ty m = Map.insert ty (TyCon n) m --this might be too strong
> makeFunMap (TyVar n) (TyVar n') m = m
> makeFunMap (TyParen t1) (TyParen t2) m = makeFunMap t1 t2 m
> makeFunMap (TyList t1) (TyList t2) m = makeFunMap t1 t2 m
> makeFunMap (TyTuple _ ts1) (TyTuple _ ts2) m =  m --if we hit a tuple give up, TODO FIX THIS
> makeFunMap (TyParArray t1) (TyParArray t2) m = makeFunMap t1 t2 m
> makeFunMap (TyApp t11 t12) (TyApp t21 t22) m = Map.union (makeFunMap t11 t21 m) (makeFunMap t12 t22 m)
> makeFunMap (TyFun t11 t12) (TyFun t21 t22) m = Map.union (makeFunMap t11 t21 m) (makeFunMap t12 t22 m)
> makeFunMap (TyKind t1 _) (TyKind t2 _) m = makeFunMap t1 t2 m
> makeFunMap (TyForall _ _ t1) (TyForall _ _ t2) m = makeFunMap t1 t2 m
> makeFunMap _ _ m = m




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

> exCurry :: Sig -> Type -> String -> Sig
> exCurry (name, _) ty x = (Ident newName, ty)
>   where newName = toString name ++ " (" ++ x ++ ")"

> coerceSig :: Type -> Sig -> [Sig]
> coerceSig target cur =
>     if isJust (isConcreteTypeOf (snd cur) target)
>     then [cur]
>     else case snd cur of
>            TyFun (TyCon (UnQual (Ident "Int"))) fn -> dfl cur fn ["-2", "-1", "0", "1", "2"]
>            TyFun (TyCon (UnQual (Ident "Integer"))) fn -> dfl cur fn ["-2", "-1", "0", "1", "2"]
>            TyFun (TyCon (UnQual (Ident "Double"))) fn -> dfl cur fn ["0.0", "1.0"]
>            TyFun (TyList _) fn -> dfl cur fn ["[]"]
>            otherwise ->  []
>   where dfl cur fn = concatMap (coerceSig target . exCurry cur fn)

> genComponentFxn :: Type -> Sig -> [Sig] -> [Sig]
> genComponentFxn exTy hofxnSig allTyps = 
>  let
>    componentSig = getComp $ snd hofxnSig :: Either String Type
>    progs = case componentSig of
>              Left e -> trace ("genComponentFxn: " ++ e ++ "\n") []
>              Right cs -> concatMap (coerceSig cs) allTyps
>  in
>    progs

> sortWith f = sortBy (compare `on` f)

> fst3 (x,_,_) = x
> snd3 (_,x,_) = x
> trd3 (_,_,x) = x
> fstsnd (x,y,_) = (x,y)
> mapSnd f = map (\(x,y) -> (x,f y))
> mapFst f = map (\(x,y) -> (f x,y))









