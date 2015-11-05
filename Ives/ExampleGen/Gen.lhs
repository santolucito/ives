> {-# LANGUAGE ExistentialQuantification #-}

> module Ives.ExampleGen.Gen (exGen, genExamples, AnyArbitrary (MkAA), Example, arguments) where

> import Ives.Types
> import Data.List
> import Data.Typeable
> import System.Random
> import Test.QuickCheck
> import Test.QuickCheck.Gen
> import Test.QuickCheck.Random

Placeholder

> exGen :: ExGen
> exGen = id

Represents an example.

> data AnyArbitrary = forall a. (Show a, Typeable a, Arbitrary a) => MkAA a
> instance Show AnyArbitrary where
>   show (MkAA a) = show a
> data AnyExampleable = forall a. Exampleable a => MkAE a
> instance Show AnyExampleable where
>   show (MkAE a) = show a
> data Example = Example { result :: AnyExampleable, arguments :: [AnyArbitrary] }
> instance Show Example where
>   show (Example res args) = unwords $ (map show args) ++ [show res]

Wrapper for intermediate functions and result.

> class (Show a) => Exampleable a where
>   examplify :: a -> QCGen -> Int -> Example

Instance for all possible return types.
If a function needs to return another type, it would need to define a new instance.
The type also needs to be a member of the show typeclass.

> instance Exampleable Bool where
>   examplify a _ _ = Example (MkAE a) []

> instance Exampleable Char where
>   examplify a _ _ = Example (MkAE a) []

> instance Exampleable Int where
>   examplify a _ _ = Example (MkAE a) []

> instance Exampleable Integer where
>   examplify a _ _ = Example (MkAE a) []

> instance Exampleable Float where
>   examplify a _ _ = Example (MkAE a) []

> instance Exampleable Double where
>   examplify a _ _ = Example (MkAE a) []

> instance Show a => Exampleable [a] where
>   examplify l _ _ = Example (MkAE l) []

> instance Exampleable () where
>   examplify a _ _ = Example (MkAE a) []

> instance (Show a, Show b) => Exampleable (a, b) where
>   examplify t _ _ = Example (MkAE t) []

> instance (Show a, Show b, Show c) => Exampleable (a, b, c) where
>   examplify t _ _ = Example (MkAE t) []

> instance (Show a, Show b, Show c, Show d) => Exampleable (a, b, c, d) where
>   examplify t _ _ = Example (MkAE t) []

Instance for intermediate curried functions.

> instance (Show a, Typeable a, Typeable b, Arbitrary a, Exampleable b) => Exampleable (a -> b) where
>   examplify f rnd size = evaluateRandom f rnd size

> instance (Typeable a, Typeable b) => Show (a -> b) where
>   show f = show $ typeOf f

Keep evaluating the function one random argument at a time.

> evaluateRandom :: (Show a, Typeable a, Arbitrary a, Exampleable b) => (a -> b) -> QCGen -> Int -> Example
> evaluateRandom body rnd0 size = res{ arguments = MkAA a : arguments res }
>   where
>     (rnd1, rnd2) = split rnd0
>     a = unGen arbitrary rnd1 size
>     res = examplify (body a) rnd2 size

Generate examples.

> genExamples :: (Exampleable a) => a -> Int -> Int -> IO [Example]
> genExamples a size n = do
>   rnd <- newQCGen
>   return $ genExamplesHelper a rnd size n

> genExamplesHelper :: (Exampleable a) => a -> QCGen -> Int -> Int -> [Example]
> genExamplesHelper _ _ _ 0 = []
> genExamplesHelper a rnd0 size n = examplify a rnd1 size : genExamplesHelper a rnd2 size (n-1)
>   where
>     (rnd1, rnd2) = split rnd0

