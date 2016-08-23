{-@ LIQUID "--no-termination" @-}

{-@ measure len @-}
len :: BTree a -> Int
len m = 
  case m of 
    Nil -> 0
    Branch t1 _ t2 -> 1 + len t1 + len t2

data BTree a = Nil | Branch (BTree a) a (BTree a) deriving Eq

double :: Int -> Int
double x = 2 * x
mult :: Int -> Int -> Int
mult = (*)

mapBTree :: (a -> a) -> BTree a -> BTree a
mapBTree f Nil = Nil
mapBTree f (Branch b1 v b2) = Branch (mapBTree f b1) (f v) (mapBTree f b2)

exs :: [(BTree Int, BTree Int)]
exs = [ ( Branch (Branch Nil 1 Nil) 3 (Branch Nil 2 Nil), 
          Branch (Branch Nil 2 Nil) 6 (Branch Nil 4 Nil) ) ]

