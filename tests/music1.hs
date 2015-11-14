{-@ LIQUID "--no-termination" @-}
import Euterpea

{-@ exs :: [(Music Pitch,Music Pitch)<{\i o -> (size i) = (size o)}>] @-}

{-@ class measure size :: forall a. a -> Int @-}

{-@ class Sized s where
      size :: forall a. x:s a -> {v:Nat | v = size x}
  @-}
class Sized s where
  size :: s a -> Int

instance Sized Music where
  {-@ instance measure size :: Music a -> Int
      size (Prim x)    = 1
      size ((:+:) m1 m2) = size m1 + size m2
      size ((:=:) m1 m2) = size m1 + size m2
      size (Modify c m) = size m
    @-}
  size = countNodes

{-@ countNodes :: m:Music a -> {v:Nat | v = size m} @-}
countNodes :: Music a -> Int
countNodes m = 
  case m of 
    Prim _  -> 1
    m1 :+: m2 -> countNodes m1 + countNodes m2
    m1 :=: m2 -> countNodes m1 + countNodes m2
    Modify c m -> countNodes m

third :: Pitch -> Pitch
third=trans 3

exs :: [(Music Pitch,Music Pitch)]
exs = [
        ((c 4 qn :+: d 4 qn), (ef 4 qn :+: f 4 qn))
      ]
