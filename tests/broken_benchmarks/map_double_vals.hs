import Data.Map.Strict as M

{-@ measure len @-}
len :: Map Int Int -> Int
len = size

double :: Int -> Int
double val = 2 * val

mkSet :: [Int] -> Map Int Int
mkSet = foldl (\m x -> M.insert x x m) M.empty

smap :: (Int -> Int) -> Map Int Int -> Map Int Int
smap = M.map

exs :: [(Map Int Int, Map Int Int)]
exs = [(mkSet [1,2,3,4], mkSet [2,4,6,8])]

