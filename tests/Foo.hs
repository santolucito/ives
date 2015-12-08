module Foo (doh) where

doh :: Int -> String
doh a
  | a > 5 = "hi"
  | a > 0 = "why"
  | otherwise = "bye"

