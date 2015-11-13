﻿{-# LANGUAGE LambdaCase #-}
import Ives.ExampleGen.Gen
import Data.Typeable
import Control.Monad

main :: IO ()
main = do
  example <- genExample (map :: (Int -> Int) -> [Int] -> [Int]) 10
  let (a0, a1, r) = extract (arguments example) (result example)
  print (a0 1)
  print a1
  print r

extract :: [AnyArbitrary] -> AnyExampleable -> ((Int -> Int), [Int], [Int])
extract args res = (a0, a1, r)
  where a0 = case args!!0 of
          MkAA a ->
            case cast a of
            Just a -> a :: Int -> Int
        a1 = case args!!1 of
          MkAA a ->
            case cast a of
            Just a -> a :: [Int]
        r = case res of
          MkAE a ->
            case cast a of
            Just a -> a :: [Int]
