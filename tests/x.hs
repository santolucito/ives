{-# LANGUAGE TemplateHaskell #-}
import Ives.ExampleGen.Gen
import Ives.ExampleGen.Conc
import Foo
import DynLoad
import System.INotify
import GHC hiding (loadModule)
import GHC.Paths (libdir)

main :: IO ()
main = do
  inotify <- initINotify
  wd <- addWatch inotify [Modify] "Foo.hs" handler
  waitForBye

waitForBye :: IO ()
waitForBye = do
  input <- getLine
  if input == "bye" then return () else waitForBye

handler :: Event -> IO ()
handler (Modified _ _) = do
  putStrLn "*** FILE UPDATED ***"
  f <- getFunc
  example <- genExample f 10
  print example

getFunc :: IO $(concretifyType 'doh)
getFunc = runGhc (Just libdir) $ do
  loadrst <- loadSourceGhc "Foo.hs"
  case loadrst of
    Just err -> error err
    Nothing  -> do
      b <- execFnGhc "Foo" "doh"
      return b

-- getExample :: IO Example
-- getExample = do
--   let f = $(send 'doh) :: $(concretify 'doh)
--   genExample f 10

-- Keeps generating random examples and printing representative ones until the function is fully covered.

-- findExamples :: FilePath -> String -> Report -> Int -> IO ()
-- findExamples executable modu prevReport count
--   | isCovered prevReport = do
--       putStrLn $ "Tried " ++ show count ++ " examples total"
--   | otherwise = do
--       example <- readProcess executable [] ""
--       report <- checkExample executable modu prevReport example
--       let newCount = count + 1
--       if newCount `mod` 1000 == 0
--         then putStrLn $ "Tried " ++ show newCount ++ " examples so far"
--         else return ()
--       findExamples executable modu report newCount

