{-# LANGUAGE ForeignFunctionInterface #-}
-- The original hpc module would only evaluate modInfo once since it was done with unsafePerformIO.
-- This version evaluates modInfo multiple times so it will report info about dynamically loaded modules.
module Ives.ExampleGen.HpcReflect (examineTix) where

import Trace.Hpc.Tix
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable ( Storable(..) )
import Data.Word
import Trace.Hpc.Util

#include "Rts.h"

foreign import ccall unsafe hs_hpc_rootModule :: IO (Ptr ())

modInfo :: IO [ModuleInfo]
modInfo = do
      ptr <- hs_hpc_rootModule
      moduleInfoList ptr

data ModuleInfo = ModuleInfo String Word32 Hash (Ptr Word64)

moduleInfoList :: Ptr () -> IO [ModuleInfo]
moduleInfoList ptr
  | ptr == nullPtr = do
        return []
  | otherwise = do
        cModName  <- (#peek HpcModuleInfo, modName) ptr
        modName   <- peekCString cModName
        tickCount <- (#peek HpcModuleInfo, tickCount) ptr
        hashNo    <- (#peek HpcModuleInfo, hashNo) ptr
        tixArr    <- (#peek HpcModuleInfo, tixArr) ptr
        next      <- (#peek HpcModuleInfo, next) ptr
        rest      <- moduleInfoList next
        return $ ModuleInfo modName tickCount (toHash (hashNo :: Int)) tixArr : rest

examineTix :: IO Tix
examineTix = do
      info <- modInfo
      mods <- sequence [ do tixs <- peekArray (fromIntegral count) ptr
                            return $ TixModule mod' hash (fromIntegral count)
                                   $ map fromIntegral tixs
                       | (ModuleInfo mod' count hash ptr) <- info
                       ]
      return $ Tix mods

