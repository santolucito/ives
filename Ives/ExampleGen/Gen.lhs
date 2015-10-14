> module Ives.ExampleGen.Gen (exGen, genExamples) where

> import Ives.Types
> import Data.List
> import System.Random
> import qualified Test.QuickCheck as QC
> import Test.QuickCheck.Gen
> import Test.QuickCheck.Random

Placeholder

> exGen :: ExGen
> exGen = id

Represents an example

> data Example
>   = Example { result :: Maybe String, arguments :: [String] }

An example without a result

> nothing :: Example
> nothing = Example { result = Nothing, arguments = [] }

Wrapper for function while generating example

> newtype Snippet
>   = Snip (Gen Example)

Wrap an example in a Snippet

> example :: Example -> Snippet
> example ex = Snip (return ex)

Wrapper for intermediate functions and result

> class Exampleable a where
>   snippet :: a -> Snippet

Instance for all possible return types
If a function needs to return another type, it would need to define a new instance.
The type also needs to be a member of the show typeclass.

> instance Exampleable Bool where
>   snippet a = example (nothing{ result = Just $ show a })

> instance Exampleable Char where
>   snippet a = example (nothing{ result = Just $ show a })

> instance Exampleable Int where
>   snippet a = example (nothing{ result = Just $ show a })

> instance Exampleable Integer where
>   snippet a = example (nothing{ result = Just $ show a })

> instance Exampleable Float where
>   snippet a = example (nothing{ result = Just $ show a })

> instance Exampleable Double where
>   snippet a = example (nothing{ result = Just $ show a })

> instance Show a => Exampleable [a] where
>   snippet l = example (nothing{ result = Just $ show l })

> instance Exampleable () where
>   snippet a = example (nothing{ result = Just $ show a })

> instance (Show a, Show b) => Exampleable (a, b) where
>   snippet t = example (nothing{ result = Just $ show t })

> instance (Show a, Show b, Show c) => Exampleable (a, b, c) where
>   snippet t = example (nothing{ result = Just $ show t })

> instance (Show a, Show b, Show c, Show d) => Exampleable (a, b, c, d) where
>   snippet t = example (nothing{ result = Just $ show t })

Instance for intermediate curried functions

> instance (QC.Arbitrary a, Show a, Exampleable b) => Exampleable (a -> b) where
>   snippet f = forAll QC.arbitrary f

Evaluate a snippet and unwrap the generator

> evaluate :: Exampleable a => a -> Gen Example
> evaluate a = gen where Snip gen = snippet a

Keep evaluating the function one random argument at a time

> forAll :: (Show a, Exampleable b) => Gen a -> (a -> b) -> Snippet
> forAll gen body = Snip $
>   do a   <- gen
>      res <- evaluate (body a)
>      return (argument a res)
>        where
>          argument a res = res{ arguments = show a : arguments res }

Generate examples

> genExamples :: (Exampleable a) => a -> Int -> Int -> IO ()
> genExamples a n size =
>   do rnd <- newQCGen
>      examplesHelper (evaluate a) rnd n size

Actual generates the examples and prints them to standard out

> examplesHelper :: Gen Example -> QCGen -> Int -> Int -> IO ()
> examplesHelper _ _ 0 _ = return ()
> examplesHelper gen rnd0 n size =
>   do putStr $ unwords ["Arguments:", args, "Result:", res]
>      putStr "\n"
>      examplesHelper gen rnd1 (n-1) size
>        where
>          (rnd1,rnd2) = split rnd0
>          example     = unGen gen rnd2 size
>          args        = show $ arguments example
>          Just res    = result example
