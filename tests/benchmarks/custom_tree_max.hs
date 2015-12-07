{-@ LIQUID "--no-termination" @-}

{-@ measure len @-}
len :: BTree a -> Int
len m = 
  case m of 
    Leaf _ -> 0
    Branch t1 t2 -> 1 + len t1 + len t2

data BTree a = Leaf a | Branch (BTree a) (BTree a) deriving Eq

add :: Int -> Int -> Int
add = (+)

accumTree :: (a -> a -> a) -> a -> BTree a -> a
accumTree f x (Leaf y) = f x y
accumTree f x (Branch b1 b2) = f (accumTree f x b1) (accumTree f x b2)

exs :: [(BTree Int, Int)]
exs = [ (Branch (Branch (Leaf 3) (Leaf 1)) (Leaf 2), 3),
        (Branch (Branch (Leaf 1) (Leaf 10)) (Leaf 5), 10),
        (Branch (Branch (Leaf 1) (Leaf 1)) (Leaf 1), 1) ]

