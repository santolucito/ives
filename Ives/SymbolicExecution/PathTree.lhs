> module Ives.SymbolicExecution.PathTree where
> import qualified Data.Map as Map
> import qualified Language.Haskell.Exts as Ext
> import qualified Ives.SymbolicExecution.AST as AST

We must think about how data is represented internally in symbolic execution.
To begin with we need to define a symbolic state that maps strings to their
respective symbolic expressions. At their core, symbolic expressions are simply
boolean constraints, which is reflected in the SymExp data constructors, which
seek to perform various function applications to represent these boolean
constraints. Note that the first two data constructors for SymExp, being ValExp
and FunExp, however, violate the notion of a boolean constraint since by
themselves they do not form a value that would evaluate to a boolean. This flaw
was implemented to make the grammar simpler for understanding. In addition, it
is possible to abuse the grammar and construct a SymExp such that a SymVal is
applied to a SymVal, which makes no semantic sense. Again, this is a flaw that
comes with making the grammar more flexible and therefore easier to process. We
hope to avoid such errors by trusting the correctness of haskell-src's parsing.

In addition, due to the way the data are defined, it is possible to classify
everything in FunVal as a SymVal, but for the sake of clarity and semantics, we
have kept them seperate.

> type SymState = Map.Map String SymExp

> data SymVal = SymVar String                     -- symbolic variable
>             | LitInt Integer                    -- integer literal
>             | LitStr String                     -- string literal
>             | LitChar Char                      -- character literal
>               deriving Show

> data FunVal = SymFun String                     -- symbolic function
>             | ExtFun [String] Ext.Exp SymState  -- Haskell Exts function
>             | DataCon String                    -- data constructor
>               deriving Show

> data SymExp = ValExp SymVal                     -- symbolic value
>             | FunExp FunVal                     -- function value / zero-arg
>             | AppExp SymExp SymExp              -- function application
>             | ParenExp SymExp                   -- parenthesized expression
>             | JoinOr SymExp SymExp              -- joint OR expressions
>               deriving Show

Some commentary about constructing path trees.
Some filler text right here.

> data PathTree = PathTerm SymExp                     -- terminal node
>               | PathSplit SymExp PathTree PathTree  -- if statement branching
>                 deriving Show

The eval function and their helper functions are aimed to evaluate the ast in
terms of symbolic expressions.

We may consider evaluating variables, literals, and data constructors as the
base case in symbolic evaluation.

> getName :: Ext.Name -> String
> getName (Ext.Ident n) = n
> getName (Ext.Symbol n) = n

> getQName :: Ext.QName -> String
> getQName (Ext.Qual (Ext.ModuleName m) n) = m ++ "." ++ getName n
> getQName (Ext.UnQual n) = getName n

> evalVar :: Ext.Exp -> SymState -> SymExp
> evalVar (Ext.Var qn) ss =
>     let res = Map.lookup (getQName qn) ss
>     in case res of
>         Just s -> s
>         Nothing -> ValExp $ SymVar $ getQName qn

> evalLit :: Ext.Exp -> SymExp
> evalLit (Ext.Lit (Ext.Int i)) = ValExp $ LitInt i
> evalLit (Ext.Lit (Ext.String s)) = ValExp $ LitStr s
> evalLit (Ext.Lit (Ext.Char c)) = ValExp $ LitChar c

> evalCon :: Ext.Exp -> SymExp
> evalCon (Ext.Con qn) = FunExp $ DataCon $ getQName qn

Something we have to consider is how we handle function calls in symbolic
evaluation. Because we begin symbolic execution seeking to return a list of
path constraints that are derived from an execution / path tree, function calls
are no exception. However, what if the function has multiple possible symbolic
expressions that it may return, we must think of a way to handle that.

One possible method is to get all the possible symbolic expression returns by
pruning the leaves of the path tree, which would be the symbolic return values
of the function calls, and then ORing them together to represent a conglomerate possibility. An additional SymExp data constructor was shoved in to accomodate
this idea. This allows us to treat multiple symbolic expressions as one.

> getPathLeaves :: PathTree -> [SymExp]
> getPathLeaves (PathTerm s) = [s]
> getPathLeaves (PathSplit _ lp rp) = (getPathLeaves lp) ++ (getPathLeaves rp) 

> joinSymExps :: [SymExp] -> SymExp
> joinSymExps [] = error "Empty SymExp list"
> joinSymExps (x:[]) = x
> joinSymExps xs = foldl JoinOr (head xs) (tail xs)

> getURhsExp :: Ext.Rhs -> Ext.Exp
> getURhsExp (Ext.UnGuardedRhs exp) = exp

> getGRhsExps :: Ext.Rhs -> [Ext.Exp]
> getGRhsExps = undefined

> getFunParams :: AST.Fun -> [String]
> getFunParams (Ext.FunBind ((Ext.Match _ _ p _ _ _):_)) =
>     map (\(Ext.PVar (Ext.Ident n)) -> n) p

> getFunExp :: AST.Fun -> Ext.Exp
> getFunExp (Ext.FunBind ((Ext.Match _ _ _ _ r _):_)) = getURhsExp r

> getCurrFun :: SymExp -> SymExp -> SymExp
> getCurrFun (FunExp (ExtFun params body ss)) arg =
>     let shadow = Map.fromList $ [(head params, arg)]
>     in FunExp $ ExtFun (tail params) body (Map.union shadow ss)

> evalFunExp :: SymExp -> AST.AST -> Ext.Exp -> SymExp
> evalFunExp fv ast arg =
>     case fv of
>         FunExp (ExtFun [] b ss) -> eval b ast ss
>         FunExp (ExtFun (x:[]) b ss) ->
>             let FunExp (ExtFun _ b ss) = getCurrFun fv (eval arg ast ss)
>             in eval b ast ss
>         FunExp (ExtFun _ b ss) -> getCurrFun fv (eval arg ast ss)

> evalInfixApp :: Ext.Exp -> AST.AST -> SymState -> SymExp
> evalInfixApp (Ext.InfixApp expL (Ext.QVarOp qn) expR) ast ss =
>     AppExp (AppExp (FunExp $ SymFun (getQName qn)) (eval expL ast ss))
>            (eval expR ast ss)

> evalApp :: Ext.Exp -> AST.AST -> SymState -> SymExp
> evalApp (Ext.App (Ext.Var qn) expR) ast ss =
>     let res = AST.lookupFun (getQName qn) ast
>     in case res of
>         Just f ->
>             let fv = FunExp $ ExtFun (getFunParams f) (getFunExp f) ss
>             in evalFunExp fv ast expR
>         Nothing -> AppExp (FunExp $ SymFun $ getQName qn) (eval expR ast ss)
> evalApp (Ext.App expL expR) ast ss =
>     AppExp (eval expL ast ss) (eval expR ast ss)

Commentary about parentheses

> evalParen :: Ext.Exp -> AST.AST -> SymState -> SymExp
> evalParen (Ext.Paren exp) ast ss = ParenExp (eval exp ast ss)

Some commentary about evaluating let expressions

> getLetLhs :: [Ext.Decl] -> [String]
> getLetLhs decls = map (\(Ext.PatBind _ (Ext.PVar n) _ _) -> getName n) decls

> getLetRhs :: [Ext.Decl] -> AST.AST -> SymState -> [SymExp]
> getLetRhs decls ast ss =
>     map (\(Ext.PatBind _ _ rhs _) -> eval (getURhsExp rhs) ast ss) decls

> evalLet :: Ext.Exp -> AST.AST -> SymState -> SymExp
> evalLet (Ext.Let (Ext.BDecls decls) exp) ast ss =
>     let res = Map.fromList $ zip (getLetLhs decls) (getLetRhs decls ast ss)
>     in eval exp ast (Map.union res ss)

Commentary about if statements

> evalIf :: Ext.Exp -> AST.AST -> SymState -> PathTree
> evalIf (Ext.If expCond expTrue expFalse) ast ss =
>     PathSplit (eval expCond ast ss)
>               (consTree expTrue ast ss)
>               (consTree expFalse ast ss)

> eval :: Ext.Exp -> AST.AST -> SymState -> SymExp
> eval exp ast ss = case exp of
>     (Ext.Var _) -> evalVar exp ss
>     (Ext.Lit _) -> evalLit exp
>     (Ext.InfixApp _ _ _) -> evalInfixApp exp ast ss
>     (Ext.App _ _) -> evalApp exp ast ss
>     (Ext.Paren _) -> evalParen exp ast ss
>     (Ext.Let _ _) -> evalLet exp ast ss
>     (Ext.If _ _ _) -> joinSymExps $ getPathLeaves $ consTree exp ast ss

Maybe have to make separate cases for these types of funciton apps etc
We may have to consider adding the function join here.

> consTree :: Ext.Exp -> AST.AST -> SymState -> PathTree
> consTree exp ast ss = case exp of
>     (Ext.If _ _ _) -> evalIf exp ast ss
>     _ -> PathTerm (eval exp ast ss)

Notes about main function we'll be exporting.

> getPathTree :: Ext.Exp -> AST.AST -> SymState -> PathTree
> getPathTree exp ast ss = consTree exp ast ss
