{-@ LIQUID "--no-termination" @-}

{-@ measure len @-}
len :: BTree a -> Int
len m = 
  case m of 
    Nil -> 1
    Branch t1 _ t2 -> 1 + len t1 + len t2

data BTree a = Nil | Branch (BTree a) a (BTree a) deriving Eq

mapBTree :: (a -> a) -> BTree a -> BTree a
mapBTree f Nil = Nil
mapBTree f (Branch b1 v b2) = Branch (mapBTree f b1) (f v) (mapBTree f b2)

exs :: [(BTree Bool, BTree Bool)]
exs = [ 
  (Branch Nil True Nil, 
  Branch Nil True Nil)]


