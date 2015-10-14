import Data.List
import Data.Maybe
import System.IO
import System.IO.Temp
import System.FilePath
import System.Directory
import System.Environment
import System.Process
import System.Exit
import Control.Exception

import System.INotify

main :: IO ()
main = do
  file:_ <- getArgs
  path <- canonicalizePath file
  inotify <- initINotify
  wd <- addWatch inotify [Modify, MoveSelf, DeleteSelf] path (handler path)
  putStrLn "Type bye to exit"
  waitForBye

waitForBye :: IO ()
waitForBye = do
  input <- getLine
  if input == "bye" then return () else waitForBye

handler :: FilePath -> Event -> IO ()
handler path (Modified _ _) = exampler path
handler _ (MovedSelf _)     = putStrLn "File was moved"
handler _ DeletedSelf       = putStrLn "File was deleted"

exampler :: FilePath -> IO ()
exampler path = do
  temp <- createTemp path
  (e, out, err) <- readProcessWithExitCode "ghc" ["-fhpc", temp] ""
  case e of
    ExitSuccess -> putStrLn out
    ExitFailure _ -> do
      putStrLn err
      exitFailure

  let program = takeBaseName temp
  let executable = joinPath [".", program]
  h <- openFile (addExtension (takeBaseName path) "examples") WriteMode
  findExamples executable h
  hClose h
  cleanup program

createTemp :: FilePath -> IO String
createTemp path = do
  contents <- readFile path
  -- remove byte order mark
  let func = dropWhile (\c -> c == '\65279') contents
  let funcName = takeWhile (\c -> c /= ' ') func
  (temp, h) <- openTempFile dir fileName
  hPutStrLn h   "import System.Environment"
  hPutStrLn h   "import Ives.ExampleGen.Gen"
  hPutStrLn h   ""
  hPutStrLn h   "main :: IO ()"
  hPutStrLn h   "main = do"
  hPutStrLn h   "  n:size:_ <- getArgs"
  hPutStrLn h $ "  genExamples " ++ funcName ++ " (read n) (read size)"
  hPutStrLn h   ""
  hPutStrLn h   func
  hClose h
  return temp
  where
    dir = takeDirectory path
    fileName = takeFileName path

findExamples :: FilePath -> Handle -> IO ()
findExamples executable h = do
  example <- readProcess executable ["1", "10"] ""
  hPutStrLn h example

cleanup :: FilePath -> IO ()
cleanup program = do
  removeFile program
  removeFile $ addExtension program "hs"
  removeFile $ addExtension program "hi"
  removeFile $ addExtension program "o"
  removeFile $ addExtension program "tix"
  removeDirectoryRecursive ".hpc"
