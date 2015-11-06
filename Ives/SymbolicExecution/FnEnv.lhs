> module Ives.SymbolicExecution.FnEnv (fnEnv) where

> import qualified Language.Haskell.Exts
> import qualified Language.Haskell.Exts as Parser
> import qualified Data.Map as Map


The Decl list can be thought of as the AST. We need to convert this into a
function lookup table for effective mock execution.

> getDecls :: String -> [Parser.Decl]
> getDecls srcString =
>     let ast = Parser.parseModule srcString
>     in case ast of
>           Parser.ParseFailed l e -> error $ "haskell-src failed:" ++ (show e)
>           Parser.ParseOk (Parser.Module _ _ _ _ _ _ d) -> d


We want to make a hashmap/table from the decl list to perform identifier lookup
In particular we are interested in functions

> isFunBind :: Parser.Decl -> Bool
> isFunBind (Parser.FunBind _) = True
> isFunBind _ = False

> fmatchIdent :: Parser.Match -> String
> fmatchIdent (Parser.Match _ (Parser.Ident name) _ _ _ _) = name


For now we do not parse guarded expressions because I decided so.

> fmatchRhs :: Parser.Match -> Parser.Exp
> fmatchRhs (Parser.Match _ _ _ _ (Parser.UnGuardedRhs exp) _) = exp

> envEntry :: Parser.Decl -> (String, [Parser.Exp])
> envEntry (Parser.FunBind ms) = ((fmatchIdent $ head ms), map fmatchRhs ms)

> parseFunBinds :: [Parser.Decl] -> [(String, [Parser.Exp])]
> parseFunBinds decls = map envEntry (filter isFunBind decls)

> fnEnv :: String -> [(String, [Parser.Exp])]
> fnEnv srcString = parseFunBinds $ getDecls srcString
