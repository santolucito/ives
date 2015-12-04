module Bar (doh, tix) where

import Trace.Hpc.Reflect
import Trace.Hpc.Tix

doh :: Int -> String
doh a
  | a == 0 = "hi"
  | otherwise = "bye"

tix :: IO Tix
tix = do
  tix <- examineTix
  print tix
  return tix

-- class Bar a where
--   derp :: a -> String
--   durr :: a -> Int

-- instance (Show a) => Bar [a] where
--   derp a = show a
--   durr a = length a

-- doh :: (Show a, Bar b) => a -> b -> String
-- doh a b
--   | durr b == 0 = show a
--   | otherwise = show a ++ derp b

