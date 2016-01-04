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


We can think of the possible execution paths as a tree. The individual path
constraints can be parsed out from the tree by taking the path from the root
to the terminal, ANDing every logical split along the way. Each place where the
path branches carries additional information about the boolean constraint logic
at the split point. A binary tree representation is valid because for now, we
assume the only splitting occurs at if statements, which have 2 possiblities.
It should be noted that it is completely possible for the conditional statement
to in itself have if statements, but we will deal with this more complicated
type of branching later. For now we assume a simple conditional.

> data PathTree = PathTerm SymExp
>               | PathSplit SymExp PathTree PathTree
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
expression evaluation. We can attempt to create a tree that represents the
execution paths, and then traverse it to parse out the path constraints (one 
path constraint would simply be the distance from the root to a leaf - where we
assume the execution stops).

For now we don't perform function lookups for function applications, but that
should be added later. For now it is basic program path exploration.

Some things of interest: https://hackage.haskell.org/package/haskell-src-exts-1.16.0.1/docs/Language-Haskell-Exts-Syntax.html#t:Exp

Because we are ultimately constructing a tree we need to think of base cases
and recursive steps. Because we are not yet performing function identifier
lookups in the environment, our base cases are thus variables, literals,
function applications and let expressions. If statements would be where we need
to consider branching.

> getName :: Exts.Name -> String
> getName (Exts.Ident n) = n
> getName (Exts.Symbol n) = n

> getRhsExp :: Exts.Rhs -> Exts.Exp
> getRhsExp (Exts.UnGuardedRhs exp) = exp
> getRhsExp (Exts.GuardedRhss ((Exts.GuardedRhs _ _ exp):xs)) = exp

> evalVar :: Exts.Exp -> SymState -> SymExp
> evalVar (Exts.Var (Exts.UnQual n)) ss =
>     let res = Map.lookup (getName n) ss
>     in case res of
>         Just s -> s
>         Nothing -> Symbol (getName n)

> evalLit :: Exts.Exp -> SymExp
> evalLit (Exts.Lit (Exts.Char c)) = LitChar c
> evalLit (Exts.Lit (Exts.String s)) = LitStr s
> evalLit (Exts.Lit (Exts.Int i)) = LitInt i

> evalInfixApp :: Exts.Exp -> SymState -> SymExp
> evalInfixApp (Exts.InfixApp expL (Exts.QVarOp (Exts.UnQual n)) expR) ss =
>     SymFnApp (SymFnApp (Symbol (getName n)) (eval expL ss)) (eval expR ss)

> evalApp :: Exts.Exp -> SymState -> SymExp
> evalApp (Exts.App expL expR) ss = SymFnApp (eval expL ss) (eval expR ss)


Let expressions are somewhat tricky since they are where we need to update the
symbolic state first before using it to evaluate the body of the corresponding
expression.

> getLetName :: [Exts.Decl] -> String
> getLetName ((Exts.PatBind _ (Exts.PVar name) _ _):xs) = getName name

> getLetSymExp :: [Exts.Decl] -> SymState -> SymExp
> getLetSymExp ((Exts.PatBind _ _ rhs _):xs) ss = eval (getRhsExp rhs) ss

> evalLet :: Exts.Exp -> SymState -> SymExp
> evalLet (Exts.Let (Exts.BDecls binds) exp) ss =
>     eval exp (Map.insert (getLetName binds) (getLetSymExp binds ss) ss)


If statements are the branching recursive cases in our execution tree. With
Haskell and pretty much every other programming language, if statements consist
of 3 parts: the condition, the true branch, and the false branch. We need to
use the condition statement as the branch marker from which we may construct a
linear path constraint.

> evalIf :: Exts.Exp -> SymState -> PathTree
> evalIf (Exts.If expCond expTrue expFalse) ss =
>     PathSplit (eval expCond ss) (consTree expTrue ss) (consTree expFalse ss)

> eval :: Exts.Exp -> SymState -> SymExp
> eval exp ss = case exp of
>     (Exts.Var _) -> evalVar exp ss
>     (Exts.Lit _) -> evalLit exp
>     (Exts.InfixApp _ _ _) -> evalInfixApp exp ss
>     (Exts.App _ _) -> evalApp exp ss
>     (Exts.Let _ _) -> evalLet exp ss

> consTree :: Exts.Exp -> SymState -> PathTree
> consTree exp ss = case exp of
>     (Exts.If _ _ _) -> evalIf exp ss
>     _ -> PathTerm (eval exp ss)

