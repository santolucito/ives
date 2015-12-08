module Foo (doh) where

doh :: Int -> String
doh a
  | a > 0 = "hi"
  | otherwise = "bye"

-- testing
