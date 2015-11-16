import System.Environment
import Ives.ExampleGen.Gen

main :: IO ()
main = do
  let n = 1
  let size = 10
  args <- getArgs
  examples <- case length args of
    1 -> genExamplesStr (args!!0) Nothing n size
    2 -> genExamplesStr (args!!0) (Just $ args!!1) n size
  print examples

-- extract :: [AnyArbitrary] -> AnyExampleable -> ((Int -> Int), [Int], [Int])
-- extract args res = (a0, a1, r)
--   where a0 = case args!!0 of
--           MkAA a ->
--             case cast a of
--             Just a -> a :: Int -> Int
--         a1 = case args!!1 of
--           MkAA a ->
--             case cast a of
--             Just a -> a :: [Int]
--         r = case res of
--           MkAE a ->
--             case cast a of
--             Just a -> a :: [Int]

