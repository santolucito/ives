import Data.Map.Strict
import Prelude hiding (map, foldr, foldl)

add :: Int -> Int -> Int
add = (+)

exs :: [(Map Int Int, Int)]
exs = [( insert 1 1 $
         insert 2 2 $
         insert 3 3 $
         insert 4 4 $
         empty
       , 10)]

