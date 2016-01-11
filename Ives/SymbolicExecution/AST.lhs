> module Ives.SymbolicExecution.AST where

> import qualified Language.Haskell.Exts as Ext


It turns out, unsurprisingly, that haskell-src's ASTs from the parseModule
function are too powerful for our needs. This module should serve to make some
prettier versions of the AST that would be useful for symbolic execution.

> type AST = [Ext.Decl]

> getAST :: String -> AST
> getAST src =
>     let res = Ext.parseModule src
>     in case res of
>         Ext.ParseFailed l e -> error $ "haskell-src failed: " ++ (show e)
>         Ext.ParseOk (Ext.Module _ _ _ _ _ _ d) -> d


It can also be useful to filter out only the function declarations (bindings).

> type Fun = Ext.Decl

> isFunBind :: Fun -> Bool
> isFunBind (Ext.FunBind _) = True
> isFunBind _ = False

> getFunList :: AST -> AST
> getFunList ast = filter isFunBind ast

> isFunMatch :: String -> Fun -> Bool
> isFunMatch d (Ext.FunBind ((Ext.Match _ (Ext.Ident n) _ _ _ _):_)) = d == n
> isFunMatch _ _ = False

> lookupFun :: String -> AST -> Maybe Fun
> lookupFun ident ast =
>     let res = filter (isFunMatch ident) (getFunList ast)
>     in case res of
>         [] -> Nothing
>         f:[] -> Just f
>         f:g:xs -> error "Too many matching identifiers"

