
{-@ map      :: (a -> b) -> xs:[a] -> {v:[b] | (len v) = (len xs)} @-}


{-@ filter :: (a -> Bool) -> xs:[a] -> {v:[a] | (len v) <= (len xs)} @-}

