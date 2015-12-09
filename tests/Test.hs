import Ives.ExampleGen.Gen
import Ives.ExampleGen.Util
import System.Environment
import Foo
import Data.Typeable

main :: IO ()
main = do
  (_, mod) <- createModule "Foo.hs" "doh"
  print $ typeOf doh
  la <- checkType mod "doh" "Int -> Char"
  print la
  cleanup mod
  return ()

