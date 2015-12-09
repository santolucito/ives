{-# LANGUAGE TemplateHaskell #-}

import Foo
import Ives.ExampleGen.Conc
import Ives.ExampleGen.Gen
import Ives.ExampleGen.Exampler
import Ives.ExampleGen.Util
import System.INotify
import System.IO
import Control.Concurrent
import Data.Typeable

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  inotify <- initINotify

  mPrev <- newEmptyMVar
  mNew <- newEmptyMVar
  maybeExamples <- tryGetExamples []
  let examples = case maybeExamples of
        Just examples -> examples
        Nothing -> []
  wd <- addWatch inotify [Modify] "Foo.hs" (handler mPrev mNew)
  loop mPrev mNew examples

loop :: MVar [Example] -> MVar [Example] -> [Example] -> IO ()
loop mPrev mNew examples = do
  putMVar mPrev examples
  newExamples <- takeMVar mNew
  loop mPrev mNew newExamples
  
handler :: MVar [Example] -> MVar [Example] -> Event -> IO ()
handler mPrev mNew _ = do
  prevExamples <- takeMVar mPrev
  maybeExamples <- tryGetExamples prevExamples
  case maybeExamples of
    Just examples -> putMVar mNew examples
    Nothing -> putMVar mNew prevExamples

tryGetExamples :: [Example] -> IO (Maybe [Example])
tryGetExamples prevExamples = do
  (moduleName, err) <- createModule "Foo.hs" "doh"
  res <- case err of
    Just errMsg -> do
      putStrLn $ "ERROR: " ++ errMsg
      return Nothing
    Nothing -> do
      same <- checkType moduleName "doh" (show $ typeOf doh)
      if not same
        then do
        putStrLn "CHANGE: Function signature changed"
        return Nothing
        else do
        (_, examples) <- getExamples moduleName "doh" prevExamples :: IO ($(concretifyType 'doh), [Example])
        putStrLn $ "EXAMPLES: " ++ show examples
        return $ Just examples
  cleanup moduleName
  return res
  
