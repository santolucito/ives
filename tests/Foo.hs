module Foo (bar) where

import Data.String.Utils

bar :: (Show a) => a -> String
bar a = show a

