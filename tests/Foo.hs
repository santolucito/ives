module Foo (doh) where

doh :: Int -> String
doh x
  | x > 9 = "hi"
  | otherwise = "bye"
