> module Ives.ExampleGen.Gen (exGen, runExample, genExamples) where

> import Ives.Types
> import Data.List
> import System.Random
> import Test.QuickCheck
> import Test.QuickCheck.Gen
> import Test.QuickCheck.Random

Placeholder

> exGen :: ExGen
> exGen = id

Represents an example.

> data Example = Example { result :: String, arguments :: [String] }

Wrapper for intermediate functions and result.

> class Exampleable a where
>   examplify :: a -> Int -> IO Example

> class ExampleReadable a where
>   readify :: a -> [String] -> IO Example

Instance for all possible return types.
If a function needs to return another type, it would need to define a new instance.
The type also needs to be a member of the show typeclass.

> instance Exampleable Bool where
>   examplify a _ = return $ Example (show a) []

> instance Exampleable Char where
>   examplify a _ = return $ Example (show a) []

> instance Exampleable Int where
>   examplify a _ = return $ Example (show a) []

> instance Exampleable Integer where
>   examplify a _ = return $ Example (show a) []

> instance Exampleable Float where
>   examplify a _ = return $ Example (show a) []

> instance Exampleable Double where
>   examplify a _ = return $ Example (show a) []

> instance Show a => Exampleable [a] where
>   examplify l _ = return $ Example (show l) []

> instance Exampleable () where
>   examplify a _ = return $ Example (show a) []

> instance (Show a, Show b) => Exampleable (a, b) where
>   examplify t _ = return $ Example (show t) []

> instance (Show a, Show b, Show c) => Exampleable (a, b, c) where
>   examplify t _ = return $ Example (show t) []

> instance (Show a, Show b, Show c, Show d) => Exampleable (a, b, c, d) where
>   examplify t _ = return $ Example (show t) []

> instance ExampleReadable Bool where
>   readify a _ = return $ Example (show a) []

> instance ExampleReadable Char where
>   readify a _ = return $ Example (show a) []

> instance ExampleReadable Int where
>   readify a _ = return $ Example (show a) []

> instance ExampleReadable Integer where
>   readify a _ = return $ Example (show a) []

> instance ExampleReadable Float where
>   readify a _ = return $ Example (show a) []

> instance ExampleReadable Double where
>   readify a _ = return $ Example (show a) []

> instance Show a => ExampleReadable [a] where
>   readify l _ = return $ Example (show l) []

> instance ExampleReadable () where
>   readify a _ = return $ Example (show a) []

> instance (Show a, Show b) => ExampleReadable (a, b) where
>   readify t _ = return $ Example (show t) []

> instance (Show a, Show b, Show c) => ExampleReadable (a, b, c) where
>   readify t _ = return $ Example (show t) []

> instance (Show a, Show b, Show c, Show d) => ExampleReadable (a, b, c, d) where
>   readify t _ = return $ Example (show t) []

Instance for intermediate curried functions.

> instance (Arbitrary a, Show a, Exampleable b) => Exampleable (a -> b) where
>   examplify f size = evaluateRandom f arbitrary size

> instance (Arbitrary a, Show a, Read a, ExampleReadable b) => ExampleReadable (a -> b) where
>   readify f args = evaluateGiven f args

> instance Show (a -> b) where
>   show f = "fn"

Keep evaluating the function one random argument at a time.

> evaluateRandom :: (Show a, Exampleable b) => (a -> b) -> Gen a -> Int -> IO Example
> evaluateRandom body gen size = do
>   a   <- generate $ resize size gen
>   res <- examplify (body a) size
>   return (argument a res)
>     where
>       argument a res = res{ arguments = show a : arguments res }

Generate examples.

> genExamples :: (Exampleable a) => a -> Int -> Int -> IO ()
> genExamples _ size 0 = return ()
> genExamples a size n = do
>   example <- examplify a size
>   putStrLn $ unwords $ (arguments example) ++ [result example]
>   genExamples a size (n-1)

Keep evaluating the function one given argument at a time.

> evaluateGiven :: (Show a, Read a, ExampleReadable b) => (a -> b) -> [String] -> IO Example
> evaluateGiven body (arg:args) = do
>   let a = read arg
>   res <- readify (body a) args
>   return (argument a res)
>     where
>       argument a res = res{ arguments = show a : arguments res }

Run a given example (the second argument is a the list of arguments to the function).

> runExample :: (ExampleReadable a) => a -> String -> IO ()
> runExample a args = do
>   example <- readify a (words args)
>   putStrLn $ unwords $ (arguments example) ++ [result example]

