> module Ives.SymbolicExecution.ASTMaker where

> import qualified Language.Haskell.Exts as Exts


It turns out, unsurprisingly, that haskell-src's ASTs from the parseModule
function are too powerful for our needs. This module should serve to make some
prettier versions of the AST that would be useful for symbolic execution.

> type AST = [Exts.Decl]

> getAST :: String -> AST
> getAST src =
>     let res = Exts.parseModule src
>     in case res of
>         Exts.ParseFailed l e -> error $ "haskell-src failed: " ++ (show e)
>         Exts.ParseOk (Exts.Module _ _ _ _ _ _ d) -> d


It can also be useful to filter out only the function declarations (bindings).

> isFunBind :: Exts.Decl -> Bool
> isFunBind (Exts.FunBind _) = True
> isFunBind _ = False

> getFnList :: AST -> AST
> getFnList ast = filter isFunBind ast


It's also useful to search the AST for a specific function by identifier.

> isFnMatch :: String -> Exts.Decl -> Bool
> isFnMatch d (Exts.FunBind ((Exts.Match _ (Exts.Ident n) _ _ _ _):_)) = d == n

> lookupFn :: String -> AST -> Exts.Decl
> lookupFn ident ast =
>     let res = filter (isFnMatch ident) (getFnList ast)
>     in case res of
>         f:[] -> f
>         f:g:xs -> error "Too many matching identifiers"
>         [] -> error "No matches found"

