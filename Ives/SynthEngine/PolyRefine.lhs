Consider the following type signatures, each of which is stronger than the previous.

1) a -> a -> a
2) Num a => a -> a -> a
3) Int -> Int -> Int

Synthesis should have two modes, one assuming user signatures are interntionally specific and the other assuming the user is not writing suficently polymorphic signatures.

--Good idea--
Beginning haskellers often do not write sufficently polymorphic signatures, choosing sig 3 when they needed sig 2.
To detect this, remove all sigs, run ghc, and compare infered types against user types.
This can help beginners learn about polymorphism.

--not really sure--
On the other hand, if the user actually knows what they are doing with signatures, we can use this information in synthesis. If the user gives a sig and examples, we might know what space of functions to search. For example,

Int -> Int -> Int
ex [(2,1)]
synth result = (\x -> x-1)

Num a => a -> a -> a
ex [(2,1)]
synth result = (\x -> x/2)


