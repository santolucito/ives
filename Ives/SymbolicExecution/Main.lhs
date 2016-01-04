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
>     -- putStrLn $ show ast
>     -- putStrLn ""
>     -- putStrLn $ show funcHue
>     -- putStrLn "" 
>     putStrLn $ show $ getExp funcHue
>     putStrLn ""
>     putStrLn $ show $ Engine.consTree (getExp funcHue) initState
