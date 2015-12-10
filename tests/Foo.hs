module Foo (doh) where

doh :: Int -> Int
doh x
  | x > 9 = 3
  | otherwise = 4
