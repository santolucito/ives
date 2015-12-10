module Foo (bar) where

bar :: (Show a) => a -> String
bar a = show a
  -- | (length . show $ a) > 2 = "ugh"
  -- | otherwise = show a

