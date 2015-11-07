> module Ives.SymbolicExecution.FnEnv (BasicFn, fnEnv) where

> import qualified Language.Haskell.Exts as Exts


The goal of this module is to create a lookup table that can return functions
when given a function's string identifier as a search key.

The BasicFn data structure represents the barebones of a function:
- identifier
- parameters
- body

> data BasicFn = BasicFn {
>     ident   :: String,
>     params  :: [Exts.Pat],
>     body    :: Exts.Exp
> } deriving (Show)


Parse the various declarations in the file of our interest.

> parseDecls :: String -> [Exts.Decl]
> parseDecls srcString =
>     let ast = Exts.parseModule srcString
>     in case ast of
>           Exts.ParseFailed l e -> error $ "haskell-src failed:" ++ (show e)
>           Exts.ParseOk (Exts.Module _ _ _ _ _ _ d) -> d


We want to check which of those declarations is a function binding, which we
are most interested in because we need those to perform mock execution.

> isFunBind :: Exts.Decl -> Bool
> isFunBind (Exts.FunBind _) = True
> isFunBind _ = False


We can extract the identifier of a function from the Exts.Match data structure.

> fnIdent :: Exts.Match -> String
> fnIdent (Exts.Match _ (Exts.Ident n) _ _ _ _) = n


A Exts.Match object is enough to allow us to construct a BasicFn.

> fnMatch :: Exts.Match -> BasicFn
> fnMatch (Exts.Match _ (Exts.Ident name) pat _ (Exts.UnGuardedRhs exp) _) =
>     BasicFn name pat exp


Each entry in the environment is mapped to potentially multiple BasicFns
because of the way Haskell does pattern matching.

> envEntry :: Exts.Decl -> (String, [BasicFn])
> envEntry (Exts.FunBind ms) =
>     let fi = ((fnIdent $ head ms))
>     in (fi, map fnMatch ms)


Filter out only the function bindings and ignore everything else.

> parseFunBinds :: [Exts.Decl] -> [(String, [BasicFn])]
> parseFunBinds decls = map envEntry (filter isFunBind decls)


Makes an environment that maps function identifiers to list of BasicFns.

> fnEnv :: String -> [(String, [BasicFn])]
> fnEnv srcString = parseFunBinds $ parseDecls srcString

