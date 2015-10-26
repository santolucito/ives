hofxn _ []     = []
hofxn f (x:xs) = (f x) : (f x ) : (map f xs)

{-@ f :: {x:(Int,Int) | fst x = snd x } @-}
f :: (Int,Int)
f = (2,2)
