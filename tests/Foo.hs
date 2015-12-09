module Foo (doh) where

doh :: Int -> Char
doh x
  | x > 9 = 'e'
  | otherwise = 'x'
