> module Ives.ExampleGen.Gen (exGen, genExamples, testExample) where

> import Ives.Types
> import Data.List
> import System.Random
> import qualified Test.QuickCheck as QC
> import Test.QuickCheck.Gen
> import Test.QuickCheck.Random

Placeholder

> exGen :: ExGen
> exGen = id

Represents an example.

> data Example = Example { result :: String, arguments :: [String] }

Wrapper for function while generating example.

> newtype Snippet
>   = Snip (Gen Example)

Wrap an example in a Snippet.

> example :: Example -> Snippet
> example ex = Snip (return ex)

Wrapper for intermediate functions and result.

> class Exampleable a where
>   snippet :: a -> [String] -> Snippet

Instance for all possible return types.
If a function needs to return another type, it would need to define a new instance.
The type also needs to be a member of the show typeclass.

> instance Exampleable Bool where
>   snippet a _ = example $ Example (show a) []

> instance Exampleable Char where
>   snippet a _ = example $ Example (show a) []

> instance Exampleable Int where
>   snippet a _ = example $ Example (show a) []

> instance Exampleable Integer where
>   snippet a _ = example $ Example (show a) []

> instance Exampleable Float where
>   snippet a _ = example $ Example (show a) []

> instance Exampleable Double where
>   snippet a _ = example $ Example (show a) []

> instance Show a => Exampleable [a] where
>   snippet l _ = example $ Example (show l) []

> instance Exampleable () where
>   snippet a _ = example $ Example (show a) []

> instance (Show a, Show b) => Exampleable (a, b) where
>   snippet t _ = example $ Example (show t) []

> instance (Show a, Show b, Show c) => Exampleable (a, b, c) where
>   snippet t _ = example $ Example (show t) []

> instance (Show a, Show b, Show c, Show d) => Exampleable (a, b, c, d) where
>   snippet t _ = example $ Example (show t) []

Instance for intermediate curried functions.

> instance (QC.Arbitrary a, Show a, Read a, Exampleable b) => Exampleable (a -> b) where
>   snippet f args
>     | null args = forAll QC.arbitrary f
>     | otherwise = forAllGiven args f

Evaluate a snippet and unwrap the generator.

> evaluate :: Exampleable a => a -> Gen Example
> evaluate a = gen where Snip gen = snippet a []

Keep evaluating the function one random argument at a time.

> forAll :: (Show a, Exampleable b) => Gen a -> (a -> b) -> Snippet
> forAll gen body = Snip $ do
>   a   <- gen
>   res <- evaluate (body a)
>   return (argument a res)
>     where
>       argument a res = res{ arguments = show a : arguments res }

Generate examples.

> genExamples :: (Exampleable a) => a -> Int -> Int -> IO ()
> genExamples a n size = do
>   rnd <- newQCGen
>   examplesHelper (evaluate a) rnd n size

Actual generates the examples and prints them to standard out.

> examplesHelper :: Gen Example -> QCGen -> Int -> Int -> IO ()
> examplesHelper _ _ 0 _ = return ()
> examplesHelper gen rnd0 n size = do
>   putStrLn $ unwords $ (arguments example) ++ [result example]
>   examplesHelper gen rnd1 (n-1) size
>     where
>       (rnd1,rnd2) = split rnd0
>       example     = unGen gen rnd2 size

Evaluate a snippet with given args and unwrap the generator.

> evaluateGiven :: Exampleable a => a -> [String] -> Gen Example
> evaluateGiven a args = gen where Snip gen = snippet a args

Keep evaluating the function one given argument at a time.

> forAllGiven :: (Show a, Read a, Exampleable b) => [String] -> (a -> b) -> Snippet
> forAllGiven (arg:args) body = Snip $ do
>   let a = read arg
>   res <- evaluateGiven (body a) args
>   return (argument a res)
>     where
>       argument a res = res{ arguments = show a : arguments res }

Test a given example (the second argument is a the list of arguments to the function).

> testExample :: (Exampleable a) => a -> [String] -> IO ()
> testExample a args = do
>   example <- generate (evaluateGiven a args)
>   putStrLn $ unwords $ (arguments example) ++ [result example]

