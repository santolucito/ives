{-# LANGUAGE TemplateHaskell #-}

import Foo
import System.FSNotify
import Ives.ExampleGen.Conc
import Ives.ExampleGen.Gen
import Ives.ExampleGen.Exampler
import System.INotify
import System.IO
import Control.Concurrent
import Data.Typeable
import Control.Monad
import Control.Exception

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  manager <- startManager
  stop <- watchDir manager "." actionFilter action

  mPrev <- newEmptyMVar
  mNew <- newEmptyMVar
  maybeExamples <- tryGetExamples "Foo.hs" "doh" (show $ typeOf doh) [] :: IO (Maybe ($(concretifyType 'doh), [Example]))
  let examples = case maybeExamples of
        Just (_, examples) -> examples
        Nothing -> []
  stop <- watchDir manager dir actionFilter (action mPrev mNew)
  catch (loop mPrev mNew examples)
    (\e -> if e == UserInterrupt
             then putStrLn "fuck"
             else throwIO e)

loop :: MVar [Example] -> MVar [Example] -> [Example] -> IO ()
loop mPrev mNew examples = do
  putMVar mPrev examples
  newExamples <- takeMVar mNew
  loop mPrev mNew newExamples
  
actionFilter :: Event -> Bool
actionFilter (Modified path _) = takeFileName path == "Foo.hs"
actionFilter _ = False

action :: MVar [Example] -> MVar [Example] -> Event -> IO ()
action mPrev mNew _ = do
  prevExamples <- takeMVar mPrev
  maybeExamples <- tryGetExamples "Foo.hs" "doh" (show $ typeOf doh) prevExamples :: IO (Maybe ($(concretifyType 'doh), [Example]))
  case maybeExamples of
    Just (_, examples) -> putMVar mNew examples
    Nothing -> putMVar mNew prevExamples

