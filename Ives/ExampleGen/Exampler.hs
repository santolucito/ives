import System.IO
import System.IO.Temp
import System.FilePath
import System.Directory
import System.Environment
import Data.List

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
  putStrLn temp
  removeFile temp

createTemp :: FilePath -> IO String
createTemp path = do
  contents <- readFile path
  -- remove byte order mark
  let func = dropWhile (\c -> c == '\65279') contents
  let funcName = takeWhile (\c -> c /= ' ') func
  (temp, handle) <- openTempFile dir fileName
  hPutStrLn handle "import Ives.ExampleGen.Gen"
  hPutStrLn handle ""
  hPutStrLn handle "main :: IO ()"
  hPutStrLn handle $ "main = do genExample " ++ funcName
  hPutStrLn handle ""
  hPutStrLn handle func
  hClose handle
  return temp
  where
    dir = takeDirectory . takeDirectory . takeDirectory $ path
    fileName = takeFileName path
