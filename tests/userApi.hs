duplIf :: (a->Bool) -> [a] -> [a]
duplIf _ []     = []
duplIf f (x:xs) = if f x then x:x:(duplIf f xs) else x:(duplIf f xs)

takeWhilePlus :: (a -> Bool) -> [a] -> [a]
takeWhilePlus f [] = []
takeWhilePlus f (x:xs) = if f x then x : takeWhilePlus f xs else [x]

foo :: [Int]
foo = map (+1) [1..5]

bar :: Int -> Bool
bar = (>2)

--this should synthesize 'duplIf bar'
exs :: [([Int],[Int])]
exs = 
  [ ([1,2,3],[1,2,3,3])
  , ([1,3,5],[1,3,3,5,5])
  ]

