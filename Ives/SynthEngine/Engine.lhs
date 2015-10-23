> {-# LANGUAGE OverloadedStrings #-}
> module Ives.SynthEngine.Engine where

> import Language.Haskell.GhcMod
> import Language.Haskell.GhcMod.Monad
> import qualified Data.Text as T
> import qualified Data.Char as C
> import Language.Haskell.Exts.Parser
> import Language.Haskell.Exts

this will grab all the defintions (fxns, types, etc) from Prelude and spit them back as a string. 
I just parse this String to build a grammar of types. There is no way to go from the value level (the String of types) the the type level (Type grammar I am generating)
I think Agda would support this, and I think it is important to have, but I can exactly say why.

> b :: IO ()
> b = do
>   r <- runGmOutT defaultOptions $ runGhcModT (defaultOptions {optDetailed = True}) $ browse "base:Prelude"
>   let x = processTys  r
>   mapM_ print x 

> processTys :: (Either GhcModError String, GhcModLog) -> [Decl]
> processTys r =
>   let 
>     tys = dropTypes $ T.lines $ handle r
>     parsedTys = map parseModule (map T.unpack tys)
>   in
>     concatMap (f) parsedTys 

> f :: ParseResult Module -> [Decl]
> f m =
>   case m of
>     ParseOk a -> getCode a
>     ParseFailed l e -> []

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
