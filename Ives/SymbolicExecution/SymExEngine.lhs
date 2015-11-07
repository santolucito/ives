> module Ives.SymbolicExecution.SymExEngine where

> import qualified Ives.SymbolicExecution.FnEnv as FnEnv
> import qualified Language.Haskell.Exts as Exts


The idea here is to inject functions in lambda form as inline functions in
order to simulate execution of the program while keeping track of path
constraints generated at logical branching points. Also Exts.Lambda is annoying
and we may want to make an auxilary function that reconverts [String]s to
[Pat]s. Ughhh.

> makePats :: [String] -> [Exts.Pat]
> makePats xs = map (\x -> Exts.PVar (Exts.Ident x)) xs

> makeLambda :: FnEnv.BasicFn -> Exts.Exp
> makeLambda (FnEnv.BasicFn i p b) =
>     Exts.Lambda (Exts.SrcLoc "n/a" 0 0) (makePats p) b


We want a function that is able to apply arguments to a BasicFn by replacing
all occurences of said bonud variables within that BasicFn. Old left new right.

> expReplace :: Exts.Exp -> [(String, String)] -> Exts.Exp
> expReplace (Exts.Var (Exts.UnQual (Exts.Ident old))) table =
>     let res = lookup old table
>     in case res of
>         Just new -> Exts.Var (Exts.UnQual (Exts.Ident new))
>         Nothing  -> Exts.Var (Exts.UnQual (Exts.Ident old))

> applyArgs :: FnEnv.BasicFn -> [String] -> Exts.Exp
> applyArgs (FnEnv.BasicFn i p b) args = expReplace b (zip p args) 

