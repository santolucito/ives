> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE LambdaCase #-}

> module Ives.SynthEngine.Extractor where

> import qualified Data.Text as T
> import qualified Data.Char as C
> import Data.Either.Combinators
> import Data.Maybe

> import Language.Haskell.GhcMod
> import Language.Haskell.GhcMod.Monad
> import Language.Haskell.Exts.Parser
> import Language.Haskell.Exts

> import Ives.SynthEngine.Types

Given a module full of examples, "browse" to get all the types of examples
then "browse" the imports of that module, and base:Prelude
then match up which example types fit which functions, and see if it works by running

There is no way to go from the value level (the String of types) the the type level (Type grammar I am generating)
Agda would support this, and it would be nice to have because then I could do more integrated type manipulation.
The approach here is just fine since we put everything into the haskell-src-exts language.

 b <- getTypesFromModule "base:Prelude"

> -- | grab all the defintions (fxns, types, etc) from a module and spit them back as a string. 
> getTypesFromModule :: String -> IO(Either String [(Name,Type)])
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
> getTypesFromCode :: Code -> Either String [(Name,Type)]
> getTypesFromCode =
>   mapRight (catMaybes.map isTypeSig) . getDeclsFromCode

> -- | take code and get every Decl, a complete abstract rep of the source
> getDeclsFromCode :: Code -> Either String [Decl]
> getDeclsFromCode =
>   handleParse . parseModule 

> -- | TODO: taking head means we dont support multiple variable type sigs
> isTypeSig :: Decl -> Maybe (Name,Type)
> isTypeSig = \case
>     TypeSig _ ns t -> Just ((head ns),t)
>     otherwise -> Nothing

> handleParse :: ParseResult Module -> Either String [Decl]
> handleParse = \case
>   ParseFailed l e -> Left $ "haskell-src failed" ++ (show e)
>   ParseOk a -> Right $ getCode a

> -- | get the ident from a typSig
> getName :: Decl -> [Name]
> getName = \case
>     TypeSig _ n _-> n
>     otherwise -> [Ident "Never do this"]

> toString :: Name -> String
> toString = \case
>   Ident s -> s
>   Symbol s -> s 

from haskell-src-exts we have 
data Module = Module
  SrcLoc ModuleName [ModulePragma] (Maybe WarningText) (Maybe [ExportSpec]) [ImportDecl] [Decl]

> getCode (Module s n p w e i d) = d


================================


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

> tryAll :: [Either String b] -> String -> Either String b
> tryAll [] e = Left e
> tryAll (c:cs) e =
>   case c of 
>     Right r -> Right r --should only have one succesful result, so just stop here 
>     Left e1 -> tryAll cs (e++e1) --carry all the errors in case we fail in the end

