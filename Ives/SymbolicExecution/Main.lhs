> import qualified Language.Haskell.Exts as Exts
> import qualified Ives.SymbolicExecution.SymbolicEngine as Engine
> import qualified Ives.SymbolicExecution.ASTMaker as AST

> getExp :: Exts.Decl -> Exts.Exp
> getExp (Exts.FunBind ((Exts.Match _ _ _ _ (Exts.UnGuardedRhs exp) _):_)) = exp

> main = do
>     src <- readFile "Sample.hs"
>     let ast = AST.getAST src
>     let funcHue = AST.lookupFn "hue" ast
>     let initState = Engine.initFnSymState funcHue
>     let exp = (getExp funcHue)
>     let tree = Engine.consTree exp initState
>     let paths = Engine.getPathCons tree
>     putStrLn "Expression:" 
>     putStrLn $ show $ exp
>     putStrLn "\nSymbolic execution tree:"
>     putStrLn $ show $ tree
>     putStrLn "\nPath Constraints:"
>     putStrLn $ show $ paths
