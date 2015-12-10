module Foo (doh) where

doh :: Int -> Char
doh x
  | x > 9 = 'd'
  | otherwise = 'x' --hellod
