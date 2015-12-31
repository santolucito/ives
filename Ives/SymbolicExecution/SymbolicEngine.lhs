> module Ives.SymbolicExecution.SymbolicEngine where

> import qualified Language.Haskell.Exts as Exts
> import qualified Data.Map as Map


We want to construct symbolic expressions, ya?
We don't really care about complexity for now because we can assume that the
SMT solver has smart optimizations :^)

We define a symbolic expression recursively. A symbolic expression may either
be a single symbol or defined in terms of a function application. This is a 
valid approach because in symbolic execution, symbolic values would either
themselves be symbols, or be defined in terms of other symbols.

In the case where we want to parse a variable whose symbolic value is defined
in terms of a function application of other symbolic values, we can pick apart
the function identifier through recursively parsing the LHS of the SymFnApp
constructor, as well as counting argument number through recursion depth.

> data SymExp = Symbol String
>             | SymFnApp SymExp SymExp
>             | LitInt Integer
>             | LitStr String
>             | LitChar Char
>               deriving Show


In addition to symbolic expressions we care about the path constraint of an
execution, which is represented in terms of symbolic expressions. The result
of a symbolic execution should be a list of path constraints mapped to their
respective outputs. We define a single path constraint as follows:

> data PathCons = PathUnit SymExp
>               | PathAnd PathCons PathCons
>               | PathOr PathCons PathCons
>               | PathNot PathCons
>                 deriving Show


The other thing we really care about in symbolic execution is symbolic state
that maps variables to their respective symbolic values - in other words their
respective symbolic expressions.

For this thing, we want to do a mapping of: String -> SymExp

The typical lookup and insert functions that we would need for this symbolic
state (which is a map / table) are already provided by the Data.Map library, so
we don't have explicitly make them (but that would be nice ... and verbose).

> type SymState = Map.Map String SymExp

> makeSymState :: [String] -> [SymExp] -> SymState
> makeSymState vars exps = Map.fromList $ zip vars exps 


The "base case" of a symbolic execution is going to be a single function, since
we can extend this concept to multiple functions (functions calling each other)
and eventually a program as a whole, which is roughly the main function calling
a lot of other functions.

In symbolic execution, the "variables" of the run are inputs to the program,
which may change in each execution. In the case of a single function, the
"variables" would be the parameters of the function. This makes sense because
the only things that we can change each call are the arguments that we pass.

As such, one thing that we are intersted in is initializing a symbolic state
for a particular function as represented by an AST made from haskell-src.

> type Func = Exts.Decl

> initFnSymState :: Func -> SymState
> initFnSymState (Exts.FunBind ((Exts.Match s n ps t r b):xs)) =
>     let params = map (\(Exts.PVar (Exts.Ident name)) -> name) ps
>     in makeSymState params (map (\name -> Symbol name) params)


The eval function would be using the symbolic state as the environment for
expression evaluation. We need to handle each type of expression differently,
meaning that each get their own different eval function that does not abide by
Haskell naming conventions because it's easier to read this way huahahua.

Some things of interest: https://hackage.haskell.org/package/haskell-src-exts-1.16.0.1/docs/Language-Haskell-Exts-Syntax.html#t:Exp

> eval_Var :: Exts.Exp -> SymState -> SymExp
> eval_Var (Exts.Var (Exts.UnQual (Exts.Ident n))) ss =
>     let res = Map.lookup n ss
>     in case res of
>         Just s -> s
>         Nothing -> Symbol n

> eval_Lit :: Exts.Exp -> SymExp
> eval_Lit (Exts.Lit (Exts.Char c)) = LitChar c
> eval_Lit (Exts.Lit (Exts.String s)) = LitStr s
> eval_Lit (Exts.Lit (Exts.Int i)) = LitInt i
