{-# LANGUAGE LambdaCase #-}

fooMap :: (a -> b) -> [a] -> [b]
fooMap f = \case
  [] -> []
  l -> map f l 
