{-# LANGUAGE ScopedTypeVariables #-}
{-@ LIQUID "--no-termination" @-}
module Class () where

import Language.Haskell.Liquid.Prelude
import Prelude hiding (sum, length, (!!), Functor(..))
import qualified Prelude as P
import Euterpea

-- data MList a = Nil | Cons (hd::a) (tl::(MList a)) @-}
--data MList a = Nil | Cons a (MList a)


{-@ class measure size :: forall a. a -> Int @-}

{-@ class Sized s where
      size :: forall a. x:s a -> {v:Nat | v = size x}
  @-}
class Sized s where
  size :: s a -> Int

instance Sized Music where
  {-@ instance measure size :: Music a -> Int
      size (Prim m)       = 1
      size ((:+:) m1 m2) = size m1 + size m2
      size ((:=:) m1 m2) = size m1 + size m2
      size (Modify c m) = size m
    @-}
  size = countNodes

{-@ countNodes :: xs:Music a -> {v:Nat | v = size xs} @-}
countNodes :: Music a -> Int
countNodes m = 
  case m of 
    Prim _  -> 1
    m1 :+: m2 -> countNodes m1 + countNodes m2
    m1 :=: m2 -> countNodes m1 + countNodes m2
    Modify c m1 -> countNodes m1

{-@ exs :: [(Music Pitch,Music Pitch)<{\i o -> (size i) = (size o)}>] @-}
exs :: [(Music Pitch,Music Pitch)]
exs = [
        ((c 4 qn), (ef 4 qn))
      ]

{-@ exs' :: [([Int],[Int])<{\i o -> (size i) = (size o)}>] @-}
exs' :: [([Int],[Int])]
exs' = [
        ([1], [1])
      ]

instance Sized [] where
  {-@ instance measure size :: [a] -> Int
      size ([])   = 0
      size (x:xs) = 1 + (size xs)
    @-}
  size [] = 0
  size (x:xs) = 1 + size xs

