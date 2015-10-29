> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE LambdaCase #-}

> module Ives.SynthEngine.TypeExtractor where

> import Language.Haskell.GhcMod
> import Language.Haskell.GhcMod.Monad
> import qualified Data.Text as T
> import qualified Data.Char as C
> import Language.Haskell.Exts.Parser
> import Language.Haskell.Exts

> import Data.Either.Combinators

Given a module full of examples, "browse" to get all the types of examples
then "browse" the imports of that module, and base:Prelude
then match up which example types fit which functions, and see if it works by running

There is no way to go from the value level (the String of types) the the type level (Type grammar I am generating)
Agda would support this, and it would be nice to have because then I could do more integrated type manipulation.
The approach here is just fine since we put everything into the haskell-src-exts language.

 b <- getTypesFromModule "base:Prelude"

> -- | grab all the defintions (fxns, types, etc) from a module and spit them back as a string. 
> getTypesFromModule :: String -> IO(Either String [Decl])
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

> -- | take code and get Decl
> getTypesFromCode :: String -> Either String [Decl]
> getTypesFromCode c =
>   let 
>     parsedC = handleParse $ parseModule $ c
>   in
>     mapRight (filter isTypeSig) parsedC

> handleParse :: ParseResult Module -> Either String [Decl]
> handleParse = \case
>   ParseFailed l e -> Left $ "haskell-src failed" ++ (show e)
>   ParseOk a -> Right $ getCode a

> isTypeSig :: Decl -> Bool
> isTypeSig = \case
>     TypeSig _ _ _-> True
>     otherwise -> False

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
