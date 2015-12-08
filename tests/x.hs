{-# LANGUAGE TemplateHaskell #-}
import Foo
import Ives.ExampleGen.Conc
import Ives.ExampleGen.Gen
import Ives.ExampleGen.Exampler
import System.INotify

main :: IO ()
main = do
  inotify <- initINotify
  wd <- addWatch inotify [Modify] "Foo.hs" handler
  waitForBye

handler :: Event -> IO ()
handler _ = do
  (_, examples) <- findExamples "Foo.hs" "doh" :: IO ($(concretifyType 'doh), [Example])
  print examples
  

waitForBye :: IO ()
waitForBye = do
  input <- getLine
  if input == "bye" then return () else waitForBye
