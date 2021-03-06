> import Data.List


MUST BE ON GHC 7.8.*

We want synthesis that learns from it's mistakes, works on the whole language, and makes idiomatic code.
1) Learn from mistakes by synthesizing sub expressions as we go. Even if a particular branch doesn't succeed, we will have done useful work.
2) Type directed synthesis based on implicit type-expansion of functions in standard library and imported libraries
3) Since humans write code using existing functions, synthesize code using existing functions.

From posera

> data Nat = O | S Nat
> data List  = Nil | Cons (Nat,List)

> stutter :: List -> List
> stutter Nil = Nil
> stutter (Cons (x,xs)) = Cons(x, Cons(x, stutter xs))

it would be better to automatically deal with all builtin types.
we also want code that is more idomatic 

> stutter' :: [a] -> [a]
> stutter' l = foldl (flip insert) [] l


we can expand the type of stutter' :: * -> *
on either the middle of left

stutter_left   :: ? -> * -> *
stutter_middle :: * -> ? -> *

the above choice illustrates expanding on the left since
foldl :: (b -> a -> b) -> b -> [a] -> b
or
foldl :: (b -> a -> b) -> b -> * -> *


using foldl we can decompose stutter' into

stutter' l = foldl f init l

we need to synth,
f    :: (b -> a -> b)
init :: b

but since b must be [a]
f    :: ([a] -> a -> [a])
init :: [a]

we can take init as constructors or variables in scope ([] or l)

for f, we have can generate contextual examples ([] -> 1 -> [1,1]) and ([1] -> 1-> [1,1])
tryign this on all functions of :: [a] -> 1 -> [1,1]

==============================

A nice way to prune the compenent search space using paramatricy of types

if we have (a->b) -> [a] -> [b], we know that the only thing that is producing the [b] is the component function.
this is nto hte case if we had the concerete type of Int, since we could have a fxn like map+1
so if the output example ([b]) is something like [1,2,3], we know that the compenent function must be one that produces positive numbers (e.g. \x -> x+1).
We can now prune the search space by removing all constant negative functions from the search space (\x -> -1)
