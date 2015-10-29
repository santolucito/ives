hofxn _ []     = []
hofxn f (x:xs) = (f x) : (f x ) : (map f xs)

