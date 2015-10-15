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
  putStrLn "Type \"bye\" to exit..."
  exampler path
  waitForBye

waitForBye :: IO ()
waitForBye = do
  input <- getLine
  if input == "bye" then return () else waitForBye

handler :: FilePath -> Event -> IO ()
handler path (Modified _ _) = do
  putStrLn "*** FILE UPDATED ***"
  exampler path
handler _ (MovedSelf _)     = do
  putStrLn "*** FILE MOVED ***"
  exitFailure
handler _ DeletedSelf       = do
  putStrLn "*** FILE DELETED ***"
  exitFailure

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
    ExitSuccess -> return ()
    ExitFailure _ -> do
      putStrLn err
      exitFailure

  let original = takeBaseName path
  let program = takeBaseName temp
  let executable = joinPath [".", program]
  let examplesPath = addExtension original "examples"
  let oldExamplesPath = addExtension examplesPath "old"
  exists <- doesFileExist examplesPath
  -- move old examples so don't overwrite
  if exists
    then do
    renameFile examplesPath oldExamplesPath
    else return ()
  h <- openFile examplesPath WriteMode
  -- run old examples if there are any
  report <- if exists
              then do
              putStrLn "Running saved examples"
              examples <- readFile oldExamplesPath
              recycleExamples executable h defaultReport (lines examples)
              else return defaultReport
  if isCovered report
    then return ()
    else do
    putStrLn "Finding new examples"
    findExamples executable h report
  putStrLn "Coverage complete"
  hClose h
  cleanup original program
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
  hPutStrLn h   "  args <- getArgs"
  hPutStrLn h $ "  example " ++ funcName ++ " args"
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

checkExample :: FilePath -> Handle -> Report -> String -> IO Report
checkExample executable h prevReport example = do
  report <- genReport executable
  if hasImproved prevReport report
    then do
    hPutStr h example
    putStr example
    else return ()
  return report
                                                            
recycleExamples :: FilePath -> Handle -> Report -> [String] -> IO Report
recycleExamples executable h prevReport (example:examples)
  | isCovered prevReport = return prevReport
  | otherwise = do
      newExample <- readProcess executable ["run", example] ""
      report <- checkExample executable h prevReport newExample
      if null examples
        then return report
        else recycleExamples executable h report examples

findExamples :: FilePath -> Handle -> Report -> IO ()
findExamples executable h prevReport
  | isCovered prevReport = return ()
  | otherwise = do
      example <- readProcess executable ["generate", "10", "1"] ""
      report <- checkExample executable h prevReport example
      findExamples executable h report

cleanup :: FilePath -> FilePath -> IO ()
cleanup original program = do
  removeFile program
  removeFile $ addExtension program "hs"
  removeFile $ addExtension program "hi"
  removeFile $ addExtension program "o"
  removeFile $ addExtension program "tix"
  let old = addExtension (addExtension original "examples") "old"
  exists <- doesFileExist old
  if exists then removeFile old else return ()
  removeDirectoryRecursive ".hpc"
