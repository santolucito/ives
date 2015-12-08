{-# LANGUAGE TemplateHaskell #-}
import Foo
import Ives.ExampleGen.Conc
import DynLoad
import Ives.ExampleGen.Gen
import Exampler
import Trace.Hpc.Reflect

main :: IO ()
main = do
  mod <- dupMod "Foo.hs"
  f <- getFunc mod "doh" :: IO $(concretifyType 'doh)
  example <- genExample f 10
  print example
  -- tix <- examineTix
  -- print tix
  -- inotify <- initINotify
  -- wd <- addWatch inotify [Modify] "Foo.hs" handler
  -- waitForBye

-- waitForBye :: IO ()
-- waitForBye = do
--   input <- getLine
--   if input == "bye" then return () else waitForBye

-- handler :: Event -> IO ()
-- handler (Modified _ _) = do
--   putStrLn "*** FILE UPDATED ***"
--   execExample

