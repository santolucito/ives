import Prelude hiding (map, foldr, foldl)
import Data.Map.Strict

type IntMap = Map Int Int

add :: Int -> Int -> Int
add = (+)

exs :: [(IntMap, Int)]
exs = [( insert 1 1 $
         insert 2 2 $
         insert 3 3 $
         insert 4 4 $
         empty
       , 10)]

