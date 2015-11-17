
f :: (Int -> Int -> Int)
f = (+)


myfold :: (a -> b -> a) -> a -> [b] -> a
myfold = foldl

x :: Int
x = 2

-- @ exs :: [([Int],Int)<{\i o -> (len i) = (len o)}>] @-} 
exs :: [([Int], Int)]
exs = [
  ([1,2,3] , 6)
  ]

prog = foldr f 0
