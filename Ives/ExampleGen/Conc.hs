{-# LANGUAGE TemplateHaskell #-}

module Ives.ExampleGen.Conc (concretify, send) where

import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

concretify :: Name -> Q Type
concretify nm = do
  info <- reify nm
  let ty = case info of
        VarI _ ty _ _ -> ty
        ClassOpI _ ty _ _ -> ty
  Just conTy <- lookupTypeName "Int"
  return $ conc (ConT conTy) ty

conc :: Type -> Type -> Type
conc conTy (VarT _) = conTy
conc conTy (ForallT _ _ ty) = conc conTy ty
conc conTy (AppT ArrowT ty1) = AppT ArrowT (conc conTy ty1)
conc conTy (AppT (VarT _) ty1) = AppT ListT (conc conTy ty1)
conc conTy (AppT ty0 ty1) = AppT (conc conTy ty0) (conc conTy ty1)
conc _ ty = ty
                              
send :: Name -> Q Exp
send nm = do
  return $ VarE nm

