xor :: Bool -> Bool -> Bool
xor a b = (not a && b) || (a && not b)

exs :: [([Bool], Bool)]
exs = [ ([True, True, True], True),
        ([False, False, False], False),
        ([True, False, True], False),
        ([True, False], True)
      ]
