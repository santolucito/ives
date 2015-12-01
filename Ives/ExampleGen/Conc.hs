module Ives.ExampleGen.Conc (concretify, send) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
                              
send :: Name -> Q Exp
send nm = do
  return $ VarE nm

concretify :: Name -> Q Type
concretify nm = do
  info <- reify nm
  let ty = case info of
        VarI _ ty _ _ -> ty
        ClassOpI _ ty _ _ -> ty
  Just conTy <- lookupTypeName "Int"
  conc Map.empty (ConT conTy) ListT ty

-- type map, default concrete type, default concrete type constructor, function type, concrete function type
conc :: (Map.Map Name Type) -> Type -> Type -> Type -> Q Type
-- replace type variable with a concrete one
conc constraints defTy defTyCon (VarT nm) = conc constraints defTy defTyCon ty
  where ty = Map.findWithDefault defTy nm constraints
conc constraints defTy defTyCon (AppT ArrowT ty) = do
  newTy <- conc constraints defTy defTyCon ty
  return $ AppT ArrowT newTy
conc constraints defTy defTyCon (AppT tyCon tyVar) = do
  -- if the type constructor is variable, use the default concrete type constructor
  newTyCon <- case tyCon of
    VarT _ -> conc constraints defTyCon defTyCon tyCon
    otherwise -> conc constraints defTy defTyCon tyCon
  newTyVar <- conc constraints defTy defTyCon tyVar
  return $ AppT newTyCon newTyVar
conc _ defTy defTyCon (ForallT _ cxt ty) = do
  constraints <- getCxtTypes cxt
  conc constraints defTy defTyCon ty
conc _ _ _ ty = return ty

getCxtTypes :: Cxt -> Q (Map.Map Name Type)
getCxtTypes cxt = do
  constraints <- mapM cxtConstraint cxt
  let instMap = processConstraints constraints Map.empty
  -- just picks the first type in the set
  return $ Map.map (\instSet -> Set.elemAt (Set.size instSet - 1) instSet) instMap
  
processConstraints :: [(Name, Set.Set Type)] -> Map.Map Name (Set.Set Type) -> Map.Map Name (Set.Set Type)
processConstraints [] m = m
processConstraints ((var, tys):xs) m = processConstraints xs newM
  where newM = Map.insertWith Set.intersection var tys m

cxtConstraint :: Pred -> Q (Name, Set.Set Type)
cxtConstraint (AppT (ConT cls) (VarT var)) = do
  tys <- getInstances cls
  return (var, Set.fromList tys)

getInstances :: Name -> Q [Type]
getInstances nm = do
  ClassI _ ins <- reify nm
  let nms = map (\(InstanceD _ (AppT _ ty) _) -> ty) ins
  return nms

