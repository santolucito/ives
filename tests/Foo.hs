module Foo (doh) where

class Bar a where
  derp :: a -> String
  durr :: a -> Int

instance (Show a) => Bar [a] where
  derp a = show a
  durr a = length a

doh :: Int -> String
doh a = show $ 2

-- doh :: (Show a, Bar b) => a -> b -> String
-- doh a b
--   | durr b == 0 = show a
--   | otherwise = show a ++ derp b

