exs :: [([Int], Bool)]
exs = [ ([2,4,6,8], True),
        ([2], True),
        ([2,3], False), -- removing this suggests any (even)
        ([1], False) ]
