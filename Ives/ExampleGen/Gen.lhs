> module Ives.ExampleGen.Gen (exGen, example) where

> import Ives.Types
> import Data.List
> import Data.Maybe
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
>   examplify :: a -> Maybe Int -> Maybe [String] -> IO Example

Instance for all possible return types.
If a function needs to return another type, it would need to define a new instance.
The type also needs to be a member of the show typeclass.

> instance Exampleable Bool where
>   examplify a _ _ = return $ Example (show a) []

> instance Exampleable Char where
>   examplify a _ _ = return $ Example (show a) []

> instance Exampleable Int where
>   examplify a _ _ = return $ Example (show a) []

> instance Exampleable Integer where
>   examplify a _ _ = return $ Example (show a) []

> instance Exampleable Float where
>   examplify a _ _ = return $ Example (show a) []

> instance Exampleable Double where
>   examplify a _ _ = return $ Example (show a) []

> instance Show a => Exampleable [a] where
>   examplify l _ _ = return $ Example (show l) []

> instance Exampleable () where
>   examplify a _ _ = return $ Example (show a) []

> instance (Show a, Show b) => Exampleable (a, b) where
>   examplify t _ _ = return $ Example (show t) []

> instance (Show a, Show b, Show c) => Exampleable (a, b, c) where
>   examplify t _ _ = return $ Example (show t) []

> instance (Show a, Show b, Show c, Show d) => Exampleable (a, b, c, d) where
>   examplify t _ _ = return $ Example (show t) []

Instance for intermediate curried functions.

> instance (Arbitrary a, Show a, Read a, Exampleable b) => Exampleable (a -> b) where
>   examplify f (Just size) _ = evaluateRandom f arbitrary size
>   examplify f _ (Just args) = evaluateGiven f args

Keep evaluating the function one random argument at a time.

> evaluateRandom :: (Show a, Exampleable b) => (a -> b) -> Gen a -> Int -> IO Example
> evaluateRandom body gen size = do
>   a   <- generate $ resize size gen
>   res <- examplify (body a) (Just size) Nothing
>   return (argument a res)
>     where
>       argument a res = res{ arguments = show a : arguments res }

Generate examples.

> genExamples :: (Exampleable a) => a -> Int -> Int -> IO ()
> genExamples _ size 0 = return ()
> genExamples a size n = do
>   example <- examplify a (Just size) Nothing
>   putStrLn $ unwords $ (arguments example) ++ [result example]
>   genExamples a size (n-1)

Keep evaluating the function one given argument at a time.

> evaluateGiven :: (Show a, Read a, Exampleable b) => (a -> b) -> [String] -> IO Example
> evaluateGiven body (arg:args) = do
>   let a = read arg
>   res <- examplify (body a) Nothing (Just args)
>   return (argument a res)
>     where
>       argument a res = res{ arguments = show a : arguments res }

Run a given example (the second argument is a the list of arguments to the function).

> runExample :: (Exampleable a) => a -> String -> IO ()
> runExample a args = do
>   example <- examplify a Nothing (Just $ words args)
>   putStrLn $ unwords $ (arguments example) ++ [result example]

> example :: (Exampleable a) => a -> [String] -> IO ()
> example a (cmd:args) =
>   case cmd of
>     "run" -> runExample a example
>       where example:_ = args
>     "generate" -> genExamples a (read size) (read n)
>       where size:n:_ = args

