This module is used to turn polymorphic functions into concrete ones.

> module Ives.ExampleGen.Conc (concretify, concretifyType) where

> import qualified Data.Map.Strict as Map
> import qualified Data.Set as Set
> import Control.Monad
> import Language.Haskell.TH
> import Language.Haskell.TH.Syntax

Since the types and type constructors that fit a certain context are found in
a unmeaningful order, we specify preferred types and type constructors to use
before resorting to the unmeaningful order.

> preferredTys :: Q [Type]
> preferredTys = do
>   Just intTy <- lookupTypeName "Int"
>   Just boolTy <- lookupTypeName "Bool"
>   Just charTy <- lookupTypeName "Char"
>   return $ map ConT [intTy, boolTy, charTy]

> preferredTyCons :: Q [Type]
> preferredTyCons = do
>   Just maybeTy <- lookupTypeName "Maybe"
>   Just eitherTy <- lookupTypeName "Either"
>   return $ ListT:(map ConT [maybeTy, eitherTy])

Turns a polymorphic function to a concrete one through template haskell.

> concretify :: Name -> Q Exp
> concretify nm = do
>   ty <- concretifyType nm
>   return $ SigE (VarE nm) ty

Returns a concrete type signature for a polymorphic function through template haskell.

> concretifyType :: Name -> Q Type
> concretifyType nm = do
>   info <- reify nm
>   let ty = case info of
>         VarI _ ty _ _ -> ty
>         ClassOpI _ ty _ _ -> ty
>   prefTys <- preferredTys
>   prefTyCons <- preferredTyCons
>   let getTy = getType prefTys Map.empty
>   let getTyCon = getType prefTyCons Map.empty
>   conc getTy getTyCon ty

Traverses the structure of the given type to replace type variables and type
constructor variables with concrete types and type constructors given a
concrete type generator and a concrete type constructor generator.

> conc :: (Name -> Type) -> (Name -> Type) -> Type -> Q Type
> conc getTy getTyCon (VarT nm) = conc getTy getTyCon ty
>   -- replace type variable with a concrete one
>   where ty = getTy nm
> conc getTy getTyCon (AppT ArrowT ty) = do
>   newTy <- conc getTy getTyCon ty
>   return $ AppT ArrowT newTy
> conc getTy getTyCon (AppT tyCon tyVar) = do
>   -- if the type constructor is variable, get a type constructor instead of a type variable
>   newTyCon <- case tyCon of
>     VarT nm -> return $ getTyCon nm
>     otherwise -> conc getTy getTyCon tyCon
>   newTyVar <- conc getTy getTyCon tyVar
>   return $ AppT newTyCon newTyVar
> conc _ _ (ForallT _ cxt ty) = do
>   constraints <- processConstraints cxt Map.empty
>   -- curry getType with preferred types and constraints
>   prefTys <- preferredTys
>   prefTyCons <- preferredTyCons
>   let getTy = getType prefTys constraints
>   let getTyCon = getType prefTyCons constraints
>   conc getTy getTyCon ty
> conc _ _ ty = return ty

Gets a concrete type for the type variable associated with the given name.
It will return one of the preferred types if possible.

> getType :: [Type] -> Map.Map Name (Set.Set Type) -> Name -> Type
> getType [] m nm = case Map.lookup nm m of
>   Just instances -> Set.elemAt (Set.size instances - 1) instances
>   Nothing -> ListT -- shouldn't ever happen
> getType (ty:tys) m nm = case Map.lookup nm m of
>   Just instances -> if Set.member ty instances then ty else getType tys m nm
>   Nothing -> ty

Processes the context of a type signature to create a map from a type variable
name to the possible concrete types it could be.

> processConstraints :: Cxt -> Map.Map Name (Set.Set Type) -> Q (Map.Map Name (Set.Set Type))
> processConstraints [] m = return m
> processConstraints (AppT (ConT cls) (VarT var):xs) m = do
>   tys <- getInstances cls
>   let newM = Map.insertWith Set.intersection var (Set.fromList tys) m
>   processConstraints xs newM

Gets all instances of a given type class.

> getInstances :: Name -> Q [Type]
> getInstances nm = do
>   ClassI _ ins <- reify nm
>   let nms = map (\(InstanceD _ (AppT _ ty) _) -> ty) ins
>   return nms

