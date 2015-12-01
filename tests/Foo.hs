module Foo (doh) where

class Bar a where
  derp :: a -> String

instance Bar (a -> b) where
  derp a = "derp"

doh :: (Show a, Bar b) => a -> b -> String
doh a b = show a ++ derp b

