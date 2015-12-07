{-# LANGUAGE ScopedTypeVariables #-}
module DynLoad (loadSourceGhc, execFnGhc) where

import qualified Data.IntSet as IntSet
import Control.Exception (throw)
import GHC hiding (loadModule)
import GHC.Paths (libdir)
import HscTypes (SourceError, srcErrorMessages)
import DynFlags
import Unsafe.Coerce
import Bag (bagToList)

execFnGhc :: String -> String -> Ghc a
execFnGhc modStr fn = do
  let modName = mkModuleName modStr
  mod <- findModule modName Nothing
  setContext [IIDecl $ simpleImportDecl modName]
  value <- compileExpr (modStr ++ "." ++ fn)
  
  let value' = (unsafeCoerce value) :: a
  return value'

loadSourceGhc :: String -> Ghc (Maybe String)
loadSourceGhc path = do
  dflags <- getSessionDynFlags
  setSessionDynFlags dflags{
    hscTarget = HscAsm,
    packageFlags = [ExposePackage (PackageArg "ghc") $ ModRenaming True []],
    generalFlags = IntSet.singleton $ fromEnum Opt_Hpc
    }
  target <- guessTarget path Nothing
  addTarget target
  r <- load LoadAllTargets
  case r of
    Failed    -> return $ Just "Generic module load error"
    Succeeded -> return Nothing

  `gcatch` \(e :: SourceError) -> let
      errors e = concat $ map show (bagToList $ srcErrorMessages e)
    in
      return $ Just (errors e)
