import Data.List
import Data.Maybe
import System.IO
import System.IO.Temp
import System.FilePath
import System.Directory
import System.Environment
import System.Process
import System.Exit
import System.Console.GetOpt
import Control.Exception
import System.INotify

data Flag = Recycle deriving (Eq, Show)

options :: [OptDescr Flag]
options = [ Option ['r'] ["recycle"] (NoArg Recycle) "Recycles examples previously generated" ]

main :: IO ()
main = do
  cliArgs <- getArgs
  (file, recycle) <- case getOpt RequireOrder options cliArgs of
    (opts, file:_, []) -> return (file, elem Recycle opts)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
      where header = "Usage: Exampler [OPTIONS] file"
  path <- canonicalizePath file
  inotify <- initINotify
  wd <- addWatch inotify [Modify, MoveSelf, DeleteSelf] path (handler path recycle)
  putStrLn "Type \"bye\" to exit..."
  exampler path recycle
  waitForBye

waitForBye :: IO ()
waitForBye = do
  input <- getLine
  if input == "bye" then return () else waitForBye

handler :: FilePath -> Bool -> Event -> IO ()
handler path recycle (Modified _ _) = do
  putStrLn "*** FILE UPDATED ***"
  exampler path recycle
handler _ _ (MovedSelf _)     = do
  error "File deleted"
handler _ _ DeletedSelf       = do
  error "File moved"

data Report = Report { coveredExpr :: Int
                     , totalExpr :: Int
                     , coveredAlt :: Int
                     , totalAlt :: Int
                     } deriving (Show)

exampler :: FilePath -> Bool -> IO ()
exampler path recycle = do
  temp <- createTemp path recycle
  let original = takeBaseName path
  let program = takeBaseName temp
  let executable = joinPath [".", program]

  (e, out, err) <- readProcessWithExitCode "ghc" ["-fhpc", temp] ""
  case e of
    ExitSuccess -> return ()
    ExitFailure _ -> do
      error err

  let examplesPath = addExtension original "examples"
  let oldExamplesPath = addExtension examplesPath "old"
  exists <- doesFileExist examplesPath

  -- move old examples so don't overwrite
  if recycle && exists
    then do
    renameFile examplesPath oldExamplesPath
    else return ()

  h <- openFile examplesPath WriteMode

  -- run old examples if there are any and recycled enabled
  report <- if recycle && exists
              then do
              putStrLn "Running saved examples"
              examples <- readFile oldExamplesPath
              recycleExamples executable h defaultReport (lines examples)
              else return defaultReport
                 
  if isCovered report
    then return ()
    else do
    putStrLn "Finding new examples"
    findExamples executable h report 1
  putStrLn "Coverage complete"
  hClose h
  cleanup original program
    where defaultReport = Report 0 1 0 1

createTemp :: FilePath -> Bool -> IO String
createTemp path recycle = do
  contents <- readFile path
  -- remove byte order mark
  let func = dropWhile (\c -> c == '\65279') contents
  let funcName = takeWhile (\c -> c /= ' ') func
  (temp, h) <- openTempFile dir fileName
  hPutStrLn h     "import System.Environment"
  hPutStrLn h     "import Ives.ExampleGen.Gen"
  hPutStrLn h     ""
  hPutStrLn h     "main :: IO ()"
  hPutStrLn h     "main = do"
  if recycle
    then do
    hPutStrLn h   "  (cmd:args) <- getArgs"
    hPutStrLn h   "  case cmd of"
    hPutStrLn h $ "    \"run\" -> runExample " ++ funcName ++ " example"
    hPutStrLn h   "      where example:_ = args"
    hPutStrLn h $ "    \"generate\" -> genExamples " ++ funcName ++ " (read size) (read n)"
    hPutStrLn h   "      where size:n:_ = args"
    else do
    hPutStrLn h   "  (_:size:n:_) <- getArgs"
    hPutStrLn h $ "  genExamples " ++ funcName ++ " (read size) (read n)"
  hPutStrLn h     ""
  hPutStrLn h     func
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

findExamples :: FilePath -> Handle -> Report -> Int -> IO ()
findExamples executable h prevReport count
  | isCovered prevReport = do
      putStrLn $ "Tried " ++ show count ++ " examples total"
  | otherwise = do
      example <- readProcess executable ["generate", "10", "1"] ""
      report <- checkExample executable h prevReport example
      if count `mod` 1000 == 0
        then putStrLn $ "Tried " ++ show count ++ " examples so far"
        else return ()
      findExamples executable h report (count + 1)

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
