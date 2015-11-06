> module Ives.SynthEngine.Types where

> data SynthSrc = HigherOrderFxn | Example
> type Code = String -- maybe use Text?

We might want to chanve the underlying rep of RType to be more similiar to Type from haskell-src-ext
http://hackage.haskell.org/package/haskell-src-exts-1.16.0.1/docs/Language-Haskell-Exts-Syntax.html#t:Type

> type RType = String

