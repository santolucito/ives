{-# LANGUAGE LambdaCase #-}

{-@ fooMap :: _ -> i:[a] -> {o:[b] | (len i) = (len o)} @-}

fooMap :: (a -> b) -> [a] -> [b]
fooMap f = \case
  [] -> []
  l -> map f l 
