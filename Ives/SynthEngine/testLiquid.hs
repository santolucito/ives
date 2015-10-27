hofxn _ []     = []
hofxn f (x:xs) = (f x) : (f x ) : (map f xs)

{-@ f :: [([Int],[Int])<{\i o -> len i = len o}>] @-}
f :: [([Int],[Int])]
f = [([2],[2])]

