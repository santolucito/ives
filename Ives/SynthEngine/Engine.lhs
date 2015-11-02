> {-# LANGUAGE LambdaCase #-}

> module Ives.SynthEngine.Engine where

> import Language.Haskell.Exts
> import qualified Data.Text as T
> import Data.List
> import Data.Either.Combinators

> import Ives.SynthEngine.RefinementTypeGen
> import Ives.SynthEngine.Extractor
> import Ives.SynthEngine.Types


the actual synth engine - take a file and generate a program for that satifies exampes given in the "exs" variable

> vroom :: String -> IO()
> vroom f = do
>   fc <- readFile f
>   let typSigs = getTypesFromCode fc
>   let exs = undefined --getExamples fc
>   case typSigs of
>     Left e -> putStrLn $ show e
>     Right typs -> proceed fc typs

> proceed fc ts = do
>   hoFxns <- genHOFxns fc ts
>   possibleCompFxns <- mapM_ (genComponentFxn fc) hoFxns
>   let validProgs = testAll (map fst hoFxns) possibleCompFxns
>   putStrLn "the following programs satisfy the examples"
>   case validProgs of
>     Left e -> putStrLn e
>     Right ps -> mapM_ putStrLn ps

once we know which hofxns we are interested in we need to generate the component function.
this should be getting the typeSig of the higherorder functions too
maybe it should also be getting the "exs" example variable
actually dont have the ability to get the code contents yet -means no way to apply functions to the examples
what we CAN do it get all the functions in scope and see which ones will fit the hofxn definition

> -- | [hofxns] -> [componentFxns] -> [validPrograms]
> testAll :: [(Name,Type)] -> [(Name,Type)] -> [String]
> testAll hs cs =
>   let
>     --x = map map 
>   in
>     "id" 


> genComponentFxn :: Code -> ((Name,Type), [(RType, RType)]) -> Either String [Type]
> genComponentFxn c r = 
>  let
>    cSig = getComp $ snd $ fst r
>    possibleFxns = case cSig of 
>      Left e -> putStrLn e
>      Right s -> foo s c
>  in
>    possibleFxns

> foo s c = do
>  case getTypesFromCode c of
>   Left e -> Left e
>   Right c' -> Right $ filter (\x -> s == snd x) c'


 -- | take all the code, and the component sig, and get the names of the fxns that fit component fxn
 foo :: [Decls] -> Type -> [String]
 foo ds csig = u 

> -- | assume that the component fxn signature will be a fxn typ surrounded by parens 
> getComp :: Type -> Either String Type
> getComp = \case
>   TyParen t1       -> case t1 of 
>                         TyFun _ _ -> Right t1 
>                         otherwise -> getComp t1
>   TyForall m c t   -> getComp t
>   TyList  t        -> getComp t
>   TyParArray t     -> getComp t
>   TyApp   t1 t2    -> getComp t2
>   TyKind  t k      -> getComp t   -- ^ type with explicit kind signature
>   TyFun t1 t2      -> tryAll [getComp t1,getComp t2] ""
>   TyTuple b ts     -> tryAll (map getComp ts) ""
>   TyVar   n        -> Left "could not find component fxn"
>   TyCon   qn       -> Left "could not find component fxn"
>   otherwise        -> Left "unsupported feature in Typsig"

the type signature datas that are curretnly unsupported
   TyInfix t1 qn t2 -> Left "unsupported feature"  -- ^ infix type constructor
   TyPromoted p     -> u -- ^ promoted data type (-XDataKinds)
   TyEquals t1 t1   -> u    -- ^ type equality predicate enabled by ConstraintKinds
   TySplice s       -> u   -- ^ template haskell splice type
   TyBang b t       -> u-- ^ Strict type marked with \"@!@\" or type marked with UNPACK pragma.

> tryAll :: [Either String b] -> String -> Either String b
> tryAll [] e = Left e
> tryAll (c:cs) e =
>   case c of 
>     Right r -> Right r --should only have one succesful result, so just stop here 
>     Left e1 -> tryAll cs (e++e1) --carry all the errors in case we fail in the end

> u = undefined 

 pl = mapM_ (putStrLn . show)
