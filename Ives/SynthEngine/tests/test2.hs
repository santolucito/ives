{-@ length' :: xs:[a] -> {v: Int | v = (len xs)} @-}
length' :: [a] -> Int
length' []     = 0
length' (x:xs) = 1 + length xs

main = return ()
