> {-# LANGUAGE ExistentialQuantification #-}

> module Ives.ExampleGen.Gen (exGen, genExample, evalExample, arguments) where

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
>   genEx :: a -> QCGen -> Int -> Example
>   evalEx :: a -> [AnyArbitrary] -> Maybe Example

Instance for all possible return types.
If a function needs to return another type, it would need to define a new instance.
The type also needs to be a member of the show typeclass.

> instance Exampleable Bool where
>   genEx a _ _ = Example (MkAE a) []
>   evalEx a _ = Just $ Example (MkAE a) []

> instance Exampleable Char where
>   genEx a _ _ = Example (MkAE a) []
>   evalEx a _ = Just $ Example (MkAE a) []

> instance Exampleable Int where
>   genEx a _ _ = Example (MkAE a) []
>   evalEx a _ = Just $ Example (MkAE a) []

> instance Exampleable Integer where
>   genEx a _ _ = Example (MkAE a) []
>   evalEx a _ = Just $ Example (MkAE a) []

> instance Exampleable Float where
>   genEx a _ _ = Example (MkAE a) []
>   evalEx a _ = Just $ Example (MkAE a) []

> instance Exampleable Double where
>   genEx a _ _ = Example (MkAE a) []
>   evalEx a _ = Just $ Example (MkAE a) []

> instance Show a => Exampleable [a] where
>   genEx l _ _ = Example (MkAE l) []
>   evalEx l _ = Just $ Example (MkAE l) []

> instance Exampleable () where
>   genEx a _ _ = Example (MkAE a) []
>   evalEx a _ = Just $ Example (MkAE a) []

> instance (Show a, Show b) => Exampleable (a, b) where
>   genEx t _ _ = Example (MkAE t) []
>   evalEx t _ = Just $ Example (MkAE t) []

> instance (Show a, Show b, Show c) => Exampleable (a, b, c) where
>   genEx t _ _ = Example (MkAE t) []
>   evalEx t _ = Just $ Example (MkAE t) []

> instance (Show a, Show b, Show c, Show d) => Exampleable (a, b, c, d) where
>   genEx t _ _ = Example (MkAE t) []
>   evalEx t _ = Just $ Example (MkAE t) []

Instance for intermediate curried functions.

> instance (Show a, Typeable a, Typeable b, Arbitrary a, Exampleable b) => Exampleable (a -> b) where
>   genEx f rnd0 size =
>     let
>       (rnd1, rnd2) = split rnd0
>       a = unGen arbitrary rnd1 size
>       res = genEx (f a) rnd2 size
>     in
>       res{ arguments = MkAA a : arguments res }
>   evalEx f ((MkAA arg):args) = do
>     a <- cast arg
>     res <- evalEx (f a) args
>     return res{ arguments = MkAA a : arguments res }
>   evalEx _ [] = Nothing

> instance (Typeable a, Typeable b) => Show (a -> b) where
>   show f = show $ typeOf f

Generate example.

> genExample :: (Exampleable a) => a -> Int -> IO Example
> genExample a size = do
>   rnd <- newQCGen
>   return $ genEx a rnd size

Evaluate example.

> evalExample :: (Exampleable a) => a -> [AnyArbitrary] -> Maybe Example
> evalExample a args = evalEx a args

