{-@ LIQUID "--no-termination" @-}
import Euterpea

{-@ measure len @-}
len :: Music a -> Int
len m = 
  case m of 
    Prim _  -> 1
    m1 :+: m2 -> len m1 + len m2
    m1 :=: m2 -> len m1 + len m2
    Modify c m -> len m


third :: Pitch -> Pitch
third=trans 3

n = c 4 qn :+: d 3 qn

{-@ exs :: [(Music Pitch,Music Pitch)<{\i o -> (len i) = (len o)}>] @-}
exs :: [(Music Pitch,Music Pitch)]
exs = [
        (n, mMap third n)
      ]
       
