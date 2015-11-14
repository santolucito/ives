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

{-@ mMap :: _ -> i:Music a -> {o:Music b | (len i) = (len o)} @-}
-- mMap :: (a -> b) -> Music a -> Music b

{-@ exs :: [(Music Pitch,Music Pitch)<{\i o -> (len i) = (len o)}>] @-}
n = c 4 qn :+: d 3 qn
exs :: [(Music Pitch,Music Pitch)]
exs = [
        --((c 4 qn), (c 4 qn))
        (n, mMap third n)
      ]
       
-- ((c 4 qn :+: d 4 qn), (ef 4 qn :+: f 4 qn))
