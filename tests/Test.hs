{-# LANGUAGE TemplateHaskell #-}

import Ives.ExampleGen.DynLoad
import Ives.ExampleGen.Conc
import Foo
import Data.Typeable

main :: IO ()
main = do
  f <- getFunc "Foo" "bar" (show $ typeOf $(concretify 'bar)) :: IO $(concretifyType 'bar)
  putStrLn $ f 4
