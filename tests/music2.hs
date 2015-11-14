{-@ LIQUID "--no-termination" @-}
import Euterpea

{-@ qualif Size(v:Int, xs:Music a): v = size xs @-}

{-@ exs :: [(Music Pitch,Music Pitch)<{\i o -> (size i) = (size o)}>] @-}

{-@ measure size @-}
size :: Music a -> Int
size m = 
  case m of 
    Prim _  -> 1
    m1 :+: m2 -> size m1 + size m2
    m1 :=: m2 -> size m1 + size m2
    Modify c m -> size m

third :: Pitch -> Pitch
third=trans 3

exs :: [(Music Pitch,Music Pitch)]
exs = [
        ((c 4 qn :+: d 4 qn), (ef 4 qn :+: f 4 qn))
      ]
