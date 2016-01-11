> module Ives.SymbolicExecution.SymbolicEngine where

> import qualified Language.Haskell.Exts as Ext
> import qualified Data.Map as Map
> import qualified Ives.SymbolicExecution.AST as AST
> import qualified Ives.SymbolicExecution.PathTree as PT

> makeSymState :: [String] -> [PT.SymExp] -> PT.SymState
> makeSymState vars exps = Map.fromList $ zip vars exps 

> initFunSymState :: AST.Fun -> PT.SymState
> initFunSymState (Ext.FunBind ((Ext.Match s n ps t r b):xs)) =
>     let params = map (\(Ext.PVar (Ext.Ident name)) -> name) ps
>     in makeSymState params (map (\name -> PT.ValExp $ PT.SymVar name) params)

> data PathCons = PathUnit PT.SymExp
>               | PathAnd PathCons PathCons
>               | PathNot PathCons
>                 deriving Show

> getPathCons :: PT.PathTree -> [PathCons]
> getPathCons (PT.PathTerm exp) = [PathUnit exp]
> getPathCons (PT.PathSplit exp left right) =
>     map (\p -> PathAnd (PathUnit exp) p) (getPathCons left) ++
>     map (\p -> PathAnd (PathNot (PathUnit exp)) p) (getPathCons right)

> getExp :: Ext.Decl -> Ext.Exp
> getExp (Ext.FunBind ((Ext.Match _ _ _ _ (Ext.UnGuardedRhs exp) _):_)) = exp

> getFun :: Maybe AST.Fun -> AST.Fun
> getFun (Just f) = f
> getFun Nothing = error "No fun for you"

> execute :: String -> AST.AST -> [PathCons]
> execute id ast =
>     let fun = getFun $ AST.lookupFun id ast
>     in let (exp, ss) = (getExp fun, initFunSymState fun)
>        in getPathCons $ PT.getPathTree exp ast ss
