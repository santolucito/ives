> module Ives.SymbolicExecution.AST (getAST) where

> import Language.Haskell.Exts
> import Language.Haskell.Exts.Parser

TODO: Make an env which is a lookup table with idents as keys and ASTs as vals.

> getAST :: String -> Either String [Decl]
> getAST src =
>     getDeclList $ parseModule src

getDeclList would return the declaration list, or a fail message.

> getDeclList :: ParseResult Module -> Either String [Decl]
> getDeclList p =
>     case p of
>         ParseFailed l e -> Left $ "haskell-src failed" ++ (show e)
>         ParseOk (Module s n p w e i d) -> Right $ d

