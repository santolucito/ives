import Ives.ExampleGen.Gen
import System.Environment
import Foo

main :: IO ()
main = do
  let n = 1
  let size = 10
  args <- getArgs
  examples <- case length args of
    1 -> genExamplesStr (args!!0) Nothing n size
    2 -> genExamplesStr (args!!0) (Just $ args!!1) n size
  print examples

