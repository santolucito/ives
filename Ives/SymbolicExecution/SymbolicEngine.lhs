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

Because Exts.Name may either be a symbol or ident, but we don't really care, so
having a universal extractor is useful so we don't have as much boilerplate.

> getNameStr :: Exts.Name -> String
> getNameStr (Exts.Ident n) = n
> getNameStr (Exts.Symbol n) = n


We also want to be able to extract the expression portion of a Rhs. For guarded
Rhs there is a potential to be multiple ones since it is essentially kind of
case / pattern matching, but for now let's only extract the first one.

> getRhsExp :: Exts.Rhs -> Exts.Exp
> getRhsExp (Exts.UnGuardedRhs exp) = exp
> getRhsExp (Exts.GuardedRhss ((Exts.GuardedRhs _ _ exp):xs)) = exp


Evaluate a variable expression. If it is within the symbolic state, retrieve it
and otherwise make a new one (most likely it would be an existing function).

> eval_Var :: Exts.Exp -> SymState -> SymExp
> eval_Var (Exts.Var (Exts.UnQual n)) ss =
>     let res = Map.lookup (getNameStr n) ss
>     in case res of
>         Just s -> s
>         Nothing -> Symbol (getNameStr n)


We want to wrap literals in the symbolic expressions.

> eval_Lit :: Exts.Exp -> SymExp
> eval_Lit (Exts.Lit (Exts.Char c)) = LitChar c
> eval_Lit (Exts.Lit (Exts.String s)) = LitStr s
> eval_Lit (Exts.Lit (Exts.Int i)) = LitInt i


There are at least 2 types of function application (another one being some
"negative" application). Infix and regular application are what we care about.
However, because infix operators can be converted to prefix operators, we parse
out both as a prefix operator, because it also fits the symbolic expression DSL
better this way. By nature, infix apps are at least binary functions, so they
get an additional layering.

> eval_InfixApp :: Exts.Exp -> SymState -> SymExp
> eval_InfixApp (Exts.InfixApp expL (Exts.QVarOp (Exts.UnQual n)) expR) ss =
>     SymFnApp (SymFnApp (Symbol (getNameStr n)) (eval expL ss)) (eval expR ss)


On the contrary, because we can apply currying, a regular function application
can be thought of a single argument application (which then returns another
function to which more arguments can be applied to).

> eval_App :: Exts.Exp -> SymState -> SymExp
> eval_App (Exts.App expL expR) ss = SymFnApp (eval expL ss) (eval expR ss)


Lambda functions would be a nice feature. Might figure out some day.

> eval_Lambda (Exts.Lambda src params exp) ss = undefined


Let expressions are a place where we update the symbolic state and use the
updated one to evaluate the corresponding expression that comes with the let
expression. We want to first parse out and evaluate the binding declarations,
update the symbolic state with those, and then pass them again into eval.

For now we assume there is only a single new binding in lets.

> getLetName :: [Exts.Decl] -> String
> getLetName ((Exts.PatBind _ (Exts.PVar name) _ _):xs) = getNameStr name

> getLetSymExp :: [Exts.Decl] -> SymState -> SymExp
> getLetSymExp ((Exts.PatBind _ _ rhs _):xs) ss =
>     let exp = getRhsExp rhs
>     in eval exp ss

> eval_Let :: Exts.Exp -> SymState -> SymExp
> eval_Let (Exts.Let (Exts.BDecls binds) exp) ss =
>     let newState = Map.insert (getLetName binds) (getLetSymExp binds ss) ss
>     in eval exp newState


In if statements, we let the first return be the true path, and the second one
be the false path. 

> eval_If :: Exts.Exp -> PathCons -> SymState -> (PathCons, PathCons)
> eval_If (Exts.If cExp tExp fExp) pc ss =
>     let (cSym, tSym, fSym) = ((eval cExp ss), (eval tExp ss), (eval fExp ss))
>     in ((PathAnd (PathUnit cSym) (PathUnit tSym)),
>         (PathAnd (PathNot (PathUnit cSym)) (PathUnit fSym)))

> eval :: Exts.Exp -> SymState -> SymExp
> eval exp ss = case exp of
>     (Exts.Var _) -> eval_Var exp ss
>     (Exts.Lit _) -> eval_Lit exp
>     (Exts.InfixApp _ _ _) -> eval_InfixApp exp ss
>     (Exts.App _ _) -> eval_App exp ss
>     (Exts.Let _ _) -> eval_Let exp ss

