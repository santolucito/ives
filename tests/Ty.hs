module Ty (printType) where

import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

printType :: Name -> Q Exp
printType nm = do
  info <- reify nm
  ty <- case info of
    VarI _ ty _ _ -> return ty
    ClassOpI _ ty _ _ -> return ty
  lift $ pprint ty

