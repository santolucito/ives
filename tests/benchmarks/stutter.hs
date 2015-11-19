

-- @ mapR :: _ -> i:[a] -> {o:[b] | (len i) = (len o)} @-}

--needed when synth isnt using prelude (for speed of testing)
myId :: a -> a
myId = id

map2 :: (a->b)->[a]->[b]
map2 f []=[]
map2 f (x:xs)=
  f x:f x:map2 f xs

mapR :: (a->b)->[a]->[b]
mapR f []=[]
mapR f (x:xs)= mapR f xs ++ [f x]

f :: a -> [a]
f x = [x,x]

-- map2 id, map f
exs :: [([Int],[Int])]
exs=[ ([1, 2, 3],[1, 1, 2, 2, 3, 3]) ]
