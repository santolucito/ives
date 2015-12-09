{-# LANGUAGE TemplateHaskell #-}

import Foo
import Ives.ExampleGen.Conc
import Ives.ExampleGen.Gen
import Ives.ExampleGen.Exampler
import System.INotify
import Control.Concurrent
import Ives.ExampleGen.HpcReflect

main :: IO ()
main = do
  inotify <- initINotify
  (_, examples) <- getExamples "Foo.hs" "doh" [] :: IO ($(concretifyType 'doh), [Example])
  mPrev <- newEmptyMVar
  mNew <- newEmptyMVar
  wd <- addWatch inotify [Modify] "Foo.hs" (handler mPrev mNew)
  doIt mPrev mNew examples

doIt :: MVar [Example] -> MVar [Example] -> [Example] -> IO ()
doIt mPrev mNew examples = do
  print examples
  putMVar mPrev examples
  newExamples <- takeMVar mNew
  doIt mPrev mNew newExamples
  
handler :: MVar [Example] -> MVar [Example] -> Event -> IO ()
handler mPrev mNew _ = do
  prevExamples <- takeMVar mPrev
  (_, examples) <- getExamples "Foo.hs" "doh" prevExamples :: IO ($(concretifyType 'doh), [Example])
  putMVar mNew examples

