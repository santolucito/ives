{-# LANGUAGE TemplateHaskell #-}

import Foo
import System.FSNotify
import Ives.ExampleGen.Conc
import Ives.ExampleGen.Gen
import Ives.ExampleGen.Exampler
import System.IO
import Data.Typeable
import System.FilePath
import Control.Exception
import Control.Concurrent

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  maybeExamples <- tryGetExamples "Foo.hs" "doh" (show $ typeOf doh) [] :: IO (Maybe ($(concretifyType 'doh), [Example]))
  let examples = case maybeExamples of
        Just (_, examples) -> examples
        Nothing -> []

  mNotify <- newEmptyMVar
  manager <- startManager
  stop <- watchDir manager "." actionFilter (action mNotify)
  loop mNotify examples

loop :: MVar () -> [Example] -> IO ()
loop mNotify prevExamples = do
  _ <- takeMVar mNotify
  maybeExamples <- tryGetExamples "Foo.hs" "doh" (show $ typeOf doh) prevExamples :: IO (Maybe ($(concretifyType 'doh), [Example]))
  case maybeExamples of
    Just (_, examples) -> loop mNotify examples
    Nothing -> loop mNotify prevExamples
  
actionFilter :: Event -> Bool
actionFilter (Modified path _) = takeFileName path == "Foo.hs"
actionFilter _ = False

action :: MVar () -> Event -> IO ()
action mNotify _ = do
  putMVar mNotify ()

