
{-@ myProgram :: i:[a] -> {o:[a] | (len i) <= (len o)} @-}
myProgram :: [a] -> [a]
myProgram = foldl (\xs x -> xs++[x,x]) []

{-@ exs :: [([Int],[Int])<{\i o -> (len i) <= (len o)}>] @-}
exs :: [([Int], [Int])]
--exs = [([1, 2, 3], myProgram [1,2,3])]
exs = [([1, 2, 3], [1,2,3,4,5,6])]

