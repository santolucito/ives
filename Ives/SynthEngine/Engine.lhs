> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE LambdaCase #-}

> module Ives.SynthEngine.Engine where

> import Language.Haskell.GhcMod
> import Language.Haskell.GhcMod.Monad
> import qualified Data.Text as T
> import qualified Data.Char as C
> import Language.Haskell.Exts.Parser
> import Language.Haskell.Exts

> import Data.Either.Combinators

NOTE: this needs to be run at the top cabal directory (when using ghci at least) or ghcmod will fail

Given a module full of examples, "browse" to get all the types of examples
then "browse" the imports of that module, and base:Prelude
then match up which example types fit which functions, and see if it works by running

There is no way to go from the value level (the String of types) the the type level (Type grammar I am generating)
Agda would support this, and it would be nice to have because then I could do more integrated type manipulation.
The approach here is just fine since we put everything into the haskell-src-exts language.

 -- | takes a module name and gives back a function name that fits the examples
 vroom :: String -> String
 vroom f = getTypesFromFile


   
> b = getTypesFromModule "base:Prelude"

> -- | grab all the defintions (fxns, types, etc) from a module and spit them back as a string. 
> getTypesFromModule :: String -> IO ()
> getTypesFromModule m = do
>   r <- runGmOutT defaultOptions $ runGhcModT (defaultOptions {optDetailed = True}) $ browse m
>   code <- either (handleMod) (return. T.pack) (fst r)
>   let x = processTys $ cleanModule code
>   putStrLn $ either show show x

> handleMod :: GhcModError -> IO(T.Text)
> handleMod x = do
>   putStrLn $ "error"++show x
>   return "ghcmod failed"

> -- | modules have some odd stuff like type definitions and 
> cleanModule :: T.Text -> T.Text
> cleanModule c =
>   let
>     c' = dropWhile (C.isAsciiUpper. T.head) $ T.lines c
>     c'' = T.unlines $ filter (T.isInfixOf "::") c'
>   in
>     c''

> -- | take code and get Decl
> processTys :: T.Text -> Either String [Decl]
> processTys c =
>   let 
>     parsedC = handleParse $ parseModule $ T.unpack c
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

from haskell-src-exts we have 
data Module = Module
  SrcLoc ModuleName [ModulePragma] (Maybe WarningText) (Maybe [ExportSpec]) [ImportDecl] [Decl]

> getCode (Module s n p w e i d) = d
