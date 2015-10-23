> {-# LANGUAGE OverloadedStrings #-}
> module Ives.SynthEngine.Engine where

> import Language.Haskell.GhcMod
> import Language.Haskell.GhcMod.Monad
> import qualified Data.Text as T
> import qualified Data.Char as C
> import Language.Haskell.Exts.Parser
> import Language.Haskell.Exts

NOTE: this needs to be run at the top cabal directory (when using ghci at least) or ghcmod will fail

Given a module full of examples, "browse" to get all the types of examples
then "browse" the imports of that module, and base:Prelude
then match up which example types fit which functions, and see if it works by running

There is no way to go from the value level (the String of types) the the type level (Type grammar I am generating)
Agda would support this, and it would be nice to have because then I could do more integrated type manipulation.
The approach here is just fine since we put everything into the haskell-src-exts language.

> -- | takes a module name and gives back a function name that fits the examples
> vroom :: String -> String
> vroom = undefined

> b = getTypesFromModule "base:Prelude"

| this will grab all the defintions (fxns, types, etc) from Prelude and spit them back as a string. 
| I just parse this String to build a grammar of types. 

> getTypesFromModule :: String -> IO ()
> getTypesFromModule m = do
>   r <- runGmOutT defaultOptions $ runGhcModT (defaultOptions {optDetailed = True}) $ browse m
>   let x = processTys  r
>   mapM_ print x 

> processTys :: (Either GhcModError String, GhcModLog) -> [Decl]
> processTys r =
>   let 
>     tys = dropTypeDefs $ T.lines $ handle r
>     parsedTys = map parseModule (map T.unpack tys)
>   in
>     concatMap (f) parsedTys 

> f :: ParseResult Module -> [Decl]
> f m =
>   case m of
>     ParseOk a -> getCode a
>     ParseFailed l e -> []

> -- | TypeDefs start up Upper case, fxn types with lower
> dropTypeDefs :: [T.Text] -> [T.Text]
> dropTypeDefs = dropWhile (C.isAsciiUpper. T.head)

> handle :: (Either GhcModError String, GhcModLog) -> T.Text
> handle x =
>   case fst x of
>     Left e -> "error"
>     Right t -> T.pack t

from haskell-src-exts we have 
data Module = Module
  SrcLoc ModuleName [ModulePragma] (Maybe WarningText) (Maybe [ExportSpec]) [ImportDecl] [Decl]

> getCode (Module s n p w e i d) = d