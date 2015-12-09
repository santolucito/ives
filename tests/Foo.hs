module Foo (doh) where

doh :: Int -> Char
doh a
  | a > 5 = 'h'
  | otherwise = 'b'
-- weird
