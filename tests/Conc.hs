{-# LANGUAGE TemplateHaskell #-}

module Conc (test, send) where

import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

test :: String -> Q Type
test str = do
  Just nm <- lookupValueName str
  VarI _ ty _ _ <- reify nm
  Just conTy <- lookupTypeName "Int"
  return $ concretify (ConT conTy) ty

concretify :: Type -> Type -> Type
concretify conTy (VarT _) = conTy
concretify conTy (ForallT _ _ ty) = concretify conTy ty
concretify conTy (AppT ty0 ty1) = AppT (concretify conTy ty0) (concretify conTy ty1)
concretify _ ty = ty
                              
send :: String -> Q Exp
send str = do
  Just nm <- lookupValueName str
  return $ VarE nm

