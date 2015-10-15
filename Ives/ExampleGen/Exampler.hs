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

data Report = Report { coveredExpr :: Int
                     , totalExpr :: Int
                     , coveredAlt :: Int
                     , totalAlt :: Int
                     } deriving (Show)

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
  findExamples executable h defaultReport
  hClose h
  cleanup program
    where defaultReport = Report 0 1 0 1

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
  hPutStrLn h $ "  genExamples " ++ funcName ++ " (read size) (read n)"
  hPutStrLn h   ""
  hPutStrLn h   func
  hClose h
  return temp
  where
    dir = takeDirectory path
    fileName = takeFileName path

genReport :: FilePath -> IO Report
genReport executable = do
  report <- readProcess "hpc" ["report", "--include=Main", executable] ""
  let reportLines = lines report
  let expressions = reportLines!!0
  let alternatives = reportLines!!5
  let coveredExpr = read $ takeWhile (/= '/') $ drop 1 $ dropWhile (/= '(') expressions
  let totalExpr = read $ takeWhile (/= ')') $ drop 1 $ dropWhile (/= '/') expressions
  let coveredAlt = read $ takeWhile (/= '/') $ drop 1 $ dropWhile (/= '(') alternatives
  let totalAlt = read $ takeWhile (/= ')') $ drop 1 $ dropWhile (/= '/') alternatives
  return $ Report coveredExpr totalExpr coveredAlt totalAlt

isCovered :: Report -> Bool
isCovered (Report ce te ca ta) = ce == te && ca == ta

hasImproved :: Report -> Report -> Bool
hasImproved (Report cePrev _ caPrev _) (Report ce _ ca _) = ce > cePrev || ca > caPrev
  
findExamples :: FilePath -> Handle -> Report -> IO ()
findExamples executable h prevReport
  | isCovered prevReport = return ()
  | otherwise = do
      example <- readProcess executable ["1", "10"] ""
      report <- genReport executable
      if hasImproved prevReport report
        then do
        hPutStrLn h example
        putStrLn example
        else return ()
      findExamples executable h report

cleanup :: FilePath -> IO ()
cleanup program = do
  removeFile program
  removeFile $ addExtension program "hs"
  removeFile $ addExtension program "hi"
  removeFile $ addExtension program "o"
  removeFile $ addExtension program "tix"
  removeDirectoryRecursive ".hpc"
