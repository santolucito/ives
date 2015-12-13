This module dynamically loads modules and returns functions within them.

> {-# LANGUAGE ScopedTypeVariables #-}
> module Ives.ExampleGen.DynLoad (getFunc) where

> import qualified Data.IntSet as IntSet
> import GHC hiding (loadModule)
> import GHC.Paths (libdir)
> import DynFlags
> import Unsafe.Coerce
> import System.FilePath
> import Control.Exception

Loads the given module dynamically and returns the requested function as the
requested type.

> getFunc :: String -> String -> String -> IO a
> getFunc moduleName func ty = defaultErrorHandler defaultFatalMessager defaultFlushOut $
>    runGhc (Just libdir) $ do
>      dflags <- getSessionDynFlags
>      setSessionDynFlags dflags{
>        hscTarget = HscAsm,
>        packageFlags = [ExposePackage (PackageArg "ghc") $ ModRenaming True []],
>        generalFlags = IntSet.singleton $ fromEnum Opt_Hpc
>        }
>      target <- guessTarget (addExtension moduleName "hs") Nothing
>      setTargets [target]
>      r <- load LoadAllTargets
>      case r of
>        Failed    -> error "Compilation failed"
>        Succeeded -> do
>          setContext [IIDecl $ simpleImportDecl (mkModuleName moduleName)
>                     ,IIDecl $ simpleImportDecl (mkModuleName "Prelude")]
>          result <- compileExpr (moduleName++"."++func++" :: "++ty)
>          let result' = unsafeCoerce result :: a
>          return result'

