{-@ hofxn :: _ -> i:[a] -> {o:[b] | (len i) >= (len o)} @-}
hofxn _ []     = []
hofxn f (x:xs) = (f x) : (f x ) : (map f xs)

