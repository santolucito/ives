This module does the actual example generation via arbitrary from QuickCheck.
It also evaluates examples for functions to get new results.

> {-# LANGUAGE ExistentialQuantification, TemplateHaskell #-}

> module Ives.ExampleGen.Example (genExample, evalExample, Example (..), AnyArbitrary (MkAA), AnyExampleable (MkAE), Exampleable) where

> import Data.List
> import Data.Typeable
> import System.Random
> import System.FilePath
> import Test.QuickCheck
> import Test.QuickCheck.Gen
> import Test.QuickCheck.Random

Wrapper class for any argument value.
Arguments need to be an instance of the Show typeclass so they can be eventually displayed to the user.
Arguments need to be an instance of the Typeable typeclass so they can be cast to their actual values during example evaluation.
Arguments need to be an instance of the Arbitrary typeclass so they can be generated by QuickCheck during example generation.

> data AnyArbitrary = forall a. (Show a, Arbitrary a, Typeable a) => MkAA a
> instance Show AnyArbitrary where
>   show (MkAA a) = show a

Wrapper class for any result value.
Results need to be an instance of the Exampleable typeclass so they can be generated during example generation.

> data AnyExampleable = forall a. (Exampleable a, Typeable a) => MkAE a
> instance Show AnyExampleable where
>   show (MkAE a) = show a

Represents an example with result.

> data Example = Example { result :: AnyExampleable, arguments :: [AnyArbitrary] }
> instance Show Example where
>   show (Example res args) = unwords $ (map show args) ++ [show res]

Represents a result of a function for which examples can be generated or evaluated.
genEx generates an example for a function a given a generator and a size (used by QuickCheck to determine the size of the random values generated).
evalEx evaluates the function a given a list of arguments wrapped in the AnyArbitrary type. Since arguments have to be cast and they might not match the signature of function a, the result is a Maybe Example.

> class (Show a) => Exampleable a where
>   genEx :: a -> QCGen -> Int -> Example
>   evalEx :: a -> [AnyArbitrary] -> Maybe Example

Instances for all possible return types.
If a function needs to return another type, it would need to define an Exampleable instance for it.

Functions need to be evaluated one argument at a time to populate the arguments of the example.

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

Non-function types just return a new example with their value as the result. They mark the end of the example generation or evaluation process.

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

> instance (Show a, Typeable a) => Exampleable [a] where
>   genEx l _ _ = Example (MkAE l) []
>   evalEx l _ = Just $ Example (MkAE l) []

> instance Exampleable () where
>   genEx a _ _ = Example (MkAE a) []
>   evalEx a _ = Just $ Example (MkAE a) []

> instance (Show a, Show b, Typeable a, Typeable b) => Exampleable (a, b) where
>   genEx t _ _ = Example (MkAE t) []
>   evalEx t _ = Just $ Example (MkAE t) []

> instance (Show a, Show b, Show c, Typeable a, Typeable b, Typeable c) => Exampleable (a, b, c) where
>   genEx t _ _ = Example (MkAE t) []
>   evalEx t _ = Just $ Example (MkAE t) []

> instance (Show a, Show b, Show c, Show d, Typeable a, Typeable b, Typeable c, Typeable d) => Exampleable (a, b, c, d) where
>   genEx t _ _ = Example (MkAE t) []
>   evalEx t _ = Just $ Example (MkAE t) []

Show instance for functions so examples with functions can be printed.

> instance (Typeable a, Typeable b) => Show (a -> b) where
>   show f = show $ typeOf f

Generate an example for a the function.
The size argument is used by QuickCheck to determine the size of generated values.

> genExample :: (Exampleable a) => a -> Int -> IO Example
> genExample a size = do
>   rnd <- newQCGen
>   return $ genEx a rnd size

Evaluate the function with the arguments given.
Useful for recycling examples as a function changes.

> evalExample :: (Exampleable a) => a -> [AnyArbitrary] -> Maybe Example
> evalExample a args = evalEx a args

