> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE LambdaCase #-}

> module Ives.SynthEngine.Extractor where

> import qualified Data.Text as T
> import qualified Data.Char as C
> import Data.Either.Combinators
> import Data.Maybe
> import qualified Data.Map.Strict as Map
> import Control.Applicative

> import Language.Haskell.GhcMod
> import Language.Haskell.GhcMod.Monad
> import Language.Haskell.Exts.Parser
> import Language.Haskell.Exts

> import Ives.SynthEngine.Types

> import Debug.Trace


Given a module full of examples, "browse" to get all the types of examples
then "browse" the imports of that module, and base:Prelude
then match up which example types fit which functions, and see if it works by running

There is no way to go from the value level (the String of types) the the type level (Type grammar I am generating)
Agda would support this, and it would be nice to have because then I could do more integrated type manipulation.
The approach here is just fine since we put everything into the haskell-src-exts language.

> -- | grab all the defintions (fxns, types, etc) from a module and spit them back as a string. 
> getTypesFromModule :: String -> IO([Sig])
> getTypesFromModule m = do
>   r <- runGmOutT defaultOptions $ runGhcModT (defaultOptions {optDetailed = True}) $ browse m
>   code <- either (handleMod) (return) (fst r)
>   let x = getTypesFromCode $ cleanModule code
>   return x

> handleMod :: GhcModError -> IO(String)
> handleMod x = do
>   putStrLn $ "error"++show x
>   return "ghcmod failed"

> -- | modules have some odd stuff that haskell-src won't able to deal with
> cleanModule :: String -> String
> cleanModule c =
>   let
>     f1 = dropWhile (C.isAsciiUpper. T.head) . T.lines
>     f2 = T.unlines . filter (T.isInfixOf "::")
>   in
>     T.unpack $ (f2.f1) $ T.pack c

> -- | take code and get Types
> getTypesFromCode :: Code -> [Sig]
> getTypesFromCode c =
>   let x = map isTypeSig $ fromJust $ fromCode c getCode
>   in catMaybes x


> -- | take code and get every Decl, a complete abstract rep of the source
> fromCode :: Code -> (Module -> a) -> Maybe a
> fromCode c getterFun = 
>   case parseModuleWithMode (defaultParseMode {extensions=knownExtensionsE}) c of
>     ParseOk a -> Just $ getterFun a
>     ParseFailed l e -> error ("Parsing failed due to: "++e)

> -- | List of all known extensions, all enabled.
> knownExtensionsE :: [Extension]
> knownExtensionsE =
>   concat [ [EnableExtension x] | x <- [minBound..maxBound] ]

> -- | TODO: taking head means we dont support multiple variable type sigs
> isTypeSig :: Decl -> Maybe Sig
> isTypeSig = \case
>     TypeSig _ ns t -> Just ((head ns),t)
>     otherwise -> Nothing

> -- | get the ident from a typSig
> getName :: Decl -> [Name]
> getName = \case
>     TypeSig _ n _-> n
>     otherwise -> [Ident "Never do this"]


> showImport :: ImportDecl -> String
> showImport (ImportDecl _ mn _ _ _ _ _ _) = 
>   let getModuleName (ModuleName n) = n
>   in getModuleName mn

from haskell-src-exts we have 
data Module = Module
  SrcLoc ModuleName [ModulePragma] (Maybe WarningText) (Maybe [ExportSpec]) [ImportDecl] [Decl]

> getCode :: Module -> [Decl]
> getCode (Module s n p w e i d) = d
> getImports (Module s n p w e i d) = i


================================

> -- | if we are able to get a component fxn sig, it must be a higher order fxn
> isHigherOrder :: Type -> Bool
> isHigherOrder = isRight. getComp

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
   TyInfix t1 qn t2 -> u  -- ^ infix type constructor
   TyPromoted p     -> u -- ^ promoted data type (-XDataKinds)
   TyEquals t1 t1   -> u    -- ^ type equality predicate enabled by ConstraintKinds
   TySplice s       -> u   -- ^ template haskell splice type
   TyBang b t       -> u-- ^ Strict type marked with \"@!@\" or type marked with UNPACK pragma.

> tyFunToList :: Type -> [Type]
> tyFunToList ty = case ty of 
>   TyFun t1 t2      -> tyFunToList t1 ++ tyFunToList t2
>   TyForall m c t   -> tyFunToList t
>   TyParArray t     -> tyFunToList t
>   TyApp   t1 t2    -> tyFunToList t2
>   TyKind  t k      -> tyFunToList t  
>   TyParen t1       -> [ty] --this will unravel the component fxns
>   TyTuple b ts     -> [TyTuple b ts] 
>   TyList  t        -> [ty]
>   TyVar   n        -> [ty]
>   TyCon   qn       -> [ty]
>   otherwise        -> [] --unsupported

 -- |if we are working witha  HOFxn like fold
 --  where we need an initial value to use it
 isInitHOFxn :: Type - Bool
 isInitHOFxn ty = 
   let 
     f t = drop 2 $ tail $ tyFunToList t
     hasInit ty = not $ null $ tyFunToList ty
   in
     isHigherOrder ty && hasInit ty

 getInitHOFxn :: Type - Type
 getInitHOFxn ty = 
   let 
     f t = drop 2 $ tail $ tyFunToList t
   in
     foldr1 TyFun $ tyFunToList ty


> exAsFunType :: Type -> Type
> exAsFunType eTy =
>   let
>     inTy  = fst $ getExType eTy
>     outTy = snd $ getExType eTy
>   in
>     TyFun inTy outTy

> getExType :: Type -> (Type,Type)
> getExType = \case
>   TyList t -> case t of 
>                 TyTuple b ts -> (ts !! 0, ts !! 1)
>                 otherwise -> error "examples are fucked"
>   otherwise -> error "examples are fucked"

> getFxnType :: Type -> (String,String)
> getFxnType s =
>   let 
>     tf = map T.strip $ T.splitOn "->" $ T.pack $ prettyPrint s
>   in 
>     (T.unpack . last $ init tf ,T.unpack $last tf)

> lastAsFunType :: Type -> Type
> lastAsFunType eTy =
>   let
>     inTy  = fst $ lastTyps eTy
>     outTy = snd $ lastTyps eTy
>   in
>     TyFun inTy outTy

> -- | get last two types
> lastTyps :: Type -> (Type,Type)
> lastTyps (TyFun t1 t2) = 
>   case t2 of
>     (TyFun t21 t22) -> lastTyps t2
>     otherwise -> (t1,t2)
> lastTyps (TyForall _ _ t) = lastTyps t
> lastTyps t = (t,t)

> lastTyp :: Type -> Type
> lastTyp (TyFun t1 t2) = lastTyp t2
> lastTyp t = t

 order matters here! this needs formlization, draw a graph
   will return ranking, on how close the match is for weighting
   would be better to have somekind of haskell poset library
   higher match is better
   would be nice to use https://hackage.haskell.org/package/hoogle-4.2.42/src/src/Hoogle/Score/Type.hs
   instead of this crappy implementation

> compareTypes :: Type -> Type -> Maybe Int
> isConcreteTypeOf = compareTypes
> compareTypes (TyParen t1) (TyParen t2) = 
>   fmap (1+) $ compareTypes t1 t2
> compareTypes (t1) (TyParen t2) = 
>   fmap (1+) $ compareTypes t1 t2
> compareTypes (TyParen t1) (t2) = 
>   fmap (1+) $ compareTypes t1 t2
> compareTypes (TyForall _ _ t1) (TyForall _ _ t2) = 
>   fmap (1+) $ compareTypes t1 t2
> compareTypes (TyForall _ _ t1) t2 = 
>   fmap (1+) $ compareTypes t1 t2
> compareTypes t1 (TyForall _ _ t2) = 
>   fmap (1+) $ compareTypes t1 t2
> compareTypes (TyList t1) (TyList t2) = 
>   fmap (1+) $ compareTypes t1 t2
> compareTypes (TyParArray t1) (TyParArray t2) =
>   fmap (1+) $ compareTypes t1 t2
> compareTypes (TyKind t1 _) (TyKind t2 _) =
>   fmap (1+) $ compareTypes t1 t2
> compareTypes (TyList t1) (TyApp t2 t2') = --[a] -> t a
>   fmap (1+) $ compareTypes t1 t2'
> compareTypes (TyTuple _ ts1) (TyTuple _ ts2) = 
>   foldl (liftA2 (+)) (Just 1) (zipWith compareTypes ts1 ts2)
> compareTypes (TyApp t1 t1') (TyApp t2 t2') =
>   fmap (1+) (liftA2 (+) (compareTypes t1 t2) (compareTypes t1' t2'))
> compareTypes (TyFun t1 t1') (TyFun t2 t2') = 
>   fmap (1+) (liftA2 (+) (compareTypes t1 t2) (compareTypes t1' t2'))
> compareTypes (TyCon q1) (TyCon q2) = if (q1==q2) then Just 20 else Nothing
> compareTypes _         (TyVar _) = Just 10
> compareTypes (TyVar _) (TyCon _) = Nothing
> compareTypes _ _ = Nothing --inlucdes unsupported types

> compareTopLevel :: Type -> Type -> Bool
> compareTopLevel (TyParen t1) (TyParen t2) = 
>   compareTopLevel t1 t2
> compareTopLevel (t1) (TyParen t2) = 
>   compareTopLevel t1 t2
> compareTopLevel (TyParen t1) (t2) = 
>   compareTopLevel t1 t2
> compareTopLevel (TyForall _ _ t1) (TyForall _ _ t2) = 
>   compareTopLevel t1 t2
> compareTopLevel (TyForall _ _ t1) t2 = 
>   compareTopLevel t1 t2
> compareTopLevel t1 (TyForall _ _ t2) = 
>   compareTopLevel t1 t2
> compareTopLevel (TyList t1) (TyList t2) = 
>   True
> compareTopLevel (TyList t1) (TyVar _) = 
>   True
> compareTopLevel (TyVar _) (TyList t1) = 
>   True
> compareTopLevel (TyParArray t1) (TyParArray t2) =
>   True
> compareTopLevel (TyKind t1 _) (TyKind t2 _) =
>   compareTopLevel t1 t2
> compareTopLevel (TyTuple _ ts1) (TyTuple _ ts2) = 
>   and (zipWith compareTopLevel ts1 ts2)
> compareTopLevel (TyApp t1 t1') (TyApp t2 t2') =
>   t1==t2
> compareTopLevel (TyFun t1 t1') (TyFun t2 t2') = 
>   (compareTopLevel t1 t2) && (compareTopLevel t1' t2')
> compareTopLevel (TyCon q1) (TyCon q2) = q1==q2
> compareTopLevel _         (TyVar _) = True
> compareTopLevel (TyVar _) ( _) = True
> compareTopLevel _ _ = False --inlucdes unsupported types

> -- | the out type of the examples is whatever is last type in the tuples
> --   the examples will be wrapped in a a list tho
> sndTyp :: Type -> Type
> sndTyp (TyList t) = case t of
>                         TyTuple _ ts -> last ts
>                         otherwise -> t --should be error
> sndTyp x = x --this should be an error

> tryAll :: [Either String b] -> String -> Either String b
> tryAll [] e = Left e
> tryAll (c:cs) e =
>   case c of 
>     Right r -> Right r --should only have one succesful result, so just stop here 
>     Left e1 -> tryAll cs (e++e1) --carry all the errors in case we fail in the end


> -- | instantiate the tyVars in abstTy with the TyCons in concTy
> --   we assume that the concTy is the ex fxn for the abstTy (a higher order fxn)
> specializeOn :: Type -> Type -> Type
> specializeOn concTy hoTy = 
>   let
>     typeMap = makeTypeMap concTy (lastAsFunType hoTy) Map.empty
>   in
>     replaceTysIn hoTy typeMap

> replaceTysIn :: Type -> Map.Map Type Type -> Type
> replaceTysIn (TyVar n) m = fromMaybe (TyVar n)  (Map.lookup (TyVar n) m)
> replaceTysIn (TyCon n) m = TyCon n 
> replaceTysIn (TyApp t1 t2) m = TyApp (replaceTysIn t1 m) (replaceTysIn t2 m)
> replaceTysIn (TyFun t1 t2) m = TyFun (replaceTysIn t1 m) (replaceTysIn t2 m)
> replaceTysIn (TyList t) m = TyList (replaceTysIn t m)
> replaceTysIn (TyParen t) m = TyParen (replaceTysIn t m)
> replaceTysIn ty m = ty --add tuple case!

> makeTypeMap :: Type -> Type -> Map.Map Type Type -> Map.Map Type Type
> makeTypeMap (TyCon _) (TyCon _) m = m --its not gonna work out between us
> makeTypeMap (TyCon n) ty m = Map.insert ty (TyCon n) m --this might be too strong
> makeTypeMap (TyVar n) (TyVar n') m = m
> makeTypeMap (TyParen t1) (TyParen t2) m = makeTypeMap t1 t2 m
> makeTypeMap (TyList t1) (TyList t2) m = makeTypeMap t1 t2 m
> makeTypeMap (TyTuple _ ts1) (TyTuple _ ts2) m =  m --if we hit a tuple give up, TODO FIX THIS
> makeTypeMap (TyParArray t1) (TyParArray t2) m = makeTypeMap t1 t2 m
> makeTypeMap (TyApp t11 t12) (TyApp t21 t22) m = Map.union (makeTypeMap t11 t21 m) (makeTypeMap t12 t22 m)
> makeTypeMap (TyFun t11 t12) (TyFun t21 t22) m = Map.union (makeTypeMap t11 t21 m) (makeTypeMap t12 t22 m)
> makeTypeMap (TyKind t1 _) (TyKind t2 _) m = makeTypeMap t1 t2 m
> makeTypeMap (TyForall _ _ t1) (TyForall _ _ t2) m = makeTypeMap t1 t2 m
> makeTypeMap _ _ m = m


