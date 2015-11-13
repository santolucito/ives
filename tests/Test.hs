{-# LANGUAGE LambdaCase #-}
import Ives.ExampleGen.Gen
import Data.Typeable
import Control.Monad

main :: IO ()
main = do
  example <- genExample test 10
  putStrLn $ show example
  let (arg:args) = arguments example
  let a = case arg of
            MkAA a ->
              case cast a :: Maybe (Int -> Int) of
                Just c -> c
  print (a 1)

test :: (Int -> Int) -> [Int] -> [Int]
test f = \case
            [] -> map f []
            otherwise -> map f [1, 2, 3]

-- bmiTell :: Float -> String
-- bmiTell bmi
--     | bmi <= 18.5 = "You're underweight, you emo, you!"  
--     | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
--     | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
--     | otherwise   = "You're a whale, congratulations!"  

