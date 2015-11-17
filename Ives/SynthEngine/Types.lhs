> {-# LANGUAGE LambdaCase #-}
> module Ives.SynthEngine.Types where

> import Language.Haskell.Exts
> import Language.Haskell.Exts.Pretty

> data SynthSrc = HigherOrderFxn | Example
> type Code = String -- maybe use Text?

We might want to chanve the underlying rep of RType to be more similiar to Type from haskell-src-ext
http://hackage.haskell.org/package/haskell-src-exts-1.16.0.1/docs/Language-Haskell-Exts-Syntax.html#t:Type

> type RType = String

> type Sig = (Name,Type)


> toString :: Name -> String
> toString = \case
>   Ident s -> s
>   Symbol s -> s 
