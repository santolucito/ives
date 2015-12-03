> import Ives.ExampleGen.Gen
> import System.IO.Error
> import Data.List
> import Data.Maybe
> import System.IO
> import System.IO.Temp
> import System.FilePath
> import System.Directory
> import System.Environment
> import System.Process
> import System.Exit
> import System.Console.GetOpt
> import Control.Exception
> import System.INotify

Flags for the Exampler program.

> data Flag = Recycle deriving (Eq, Show)
> options :: [OptDescr Flag]
> options = [ Option ['r'] ["recycle"] (NoArg Recycle) "Recycles examples previously generated" ]

Watches a file and generates examples for all branches whenever the file changes.

> main :: IO ()
> main = do
>   cliArgs <- getArgs
>   let (opts, args, errs) = getOpt RequireOrder options cliArgs
>   let header = "Usage: Exampler [OPTIONS] function file"
>   if null errs
>     then return ()
>     else ioError $ userError $ concat errs ++ usageInfo header options
>   if null args
>     then ioError $ userError $ usageInfo header options
>     else return ()
>   let recycle = elem Recycle opts
>   let (func:file:_) = args
>   inotify <- initINotify
>   wd <- addWatch inotify [Modify, MoveSelf, DeleteSelf] file (handler func file recycle)
>   putStrLn "Type \"bye\" to exit..."
>   exampler func file recycle
>   waitForBye

Exit when the user types "bye".

> waitForBye :: IO ()
> waitForBye = do
>   input <- getLine
>   if input == "bye" then return () else waitForBye

Responds to file changes by generating new examples.

> handler :: String -> FilePath -> Bool -> Event -> IO ()
> handler func file recycle (Modified _ _) = do
>   putStrLn "*** FILE UPDATED ***"
>   exampler func file recycle
> handler _ _ _ (MovedSelf _)     = do
>   error "File moved"
> handler _ _ _ DeletedSelf       = do
>   error "File deleted"

Represents an HPC report.

> data Report = Report { coveredAlt :: Int
>                      , totalAlt :: Int
>                      } deriving (Show)

Generates examples that fully cover the function in the file corresponding to FilePath.
It prints out examples that increase coverage until coverage is complete.
When recycle is true, examples that were generated previously used will be reused as long as the signature of the function is the same other than the return type.
The recycle option does not work since the program doesn't know how to call another Haskell program with the arguments (i.e., if an argument is a function, then no way to write it to a string then read it back in another program).

> exampler :: String -> FilePath -> Bool -> IO ()
> exampler func file recycle = do
>   (temp, Just modu) <- createProgram func (Just file) 1 10 -- just one example
>   let executable = joinPath [".", takeBaseName temp]
> 
>   (e, out, err) <- readProcessWithExitCode "ghc" ["-fhpc", temp] ""
>   case e of
>     ExitSuccess -> return ()
>     ExitFailure _ -> do
>       cleanup temp modu
>       error err
> 
>   putStrLn "Finding examples"
>   let report = Report 0 1
>   findExamples executable (takeBaseName modu) report 0
>   putStrLn "Coverage complete"
>   cleanup temp modu

Generate an HPC report for a program.

> genReport :: FilePath -> String -> IO Report
> genReport executable modu = do
>   report <- readProcess "hpc" ["report", "--include="++modu, executable] ""
>   let reportLines = lines report
>   let alternatives = reportLines!!5
>   let coveredAlt = read $ takeWhile (/= '/') $ drop 1 $ dropWhile (/= '(') alternatives
>   let totalAlt = read $ takeWhile (/= ')') $ drop 1 $ dropWhile (/= '/') alternatives
>   return $ Report coveredAlt totalAlt

Checks if a report has full coverage.
Might make sense to consider a certain percent coverage as being "covered" or to let a few unexplored branches to slide if it's taking too long.

> isCovered :: Report -> Bool
> isCovered (Report ca ta) = ca == ta

Checks if the second report is an improvement over the first.

> hasImproved :: Report -> Report -> Bool
> hasImproved (Report caPrev ta) (Report ca _) = ca > caPrev || ca == ta

Checks if the provided example improved the report.
If it did, it's printed to standard out.
It's possible that an example that passed this check prior to another example that covers all branches that the earlier example covered as well. In that case, the earlier example is not needed as a representative example, but this case is not accounted for currently.

> checkExample :: FilePath -> String -> Report -> String -> IO Report
> checkExample executable modu prevReport example = do
>   report <- genReport executable modu
>   if hasImproved prevReport report
>     then putStr example
>     else return ()
>   return report

Keeps generating random examples and printing representative ones until the function is fully covered.

> findExamples :: FilePath -> String -> Report -> Int -> IO ()
> findExamples executable modu prevReport count
>   | isCovered prevReport = do
>       putStrLn $ "Tried " ++ show count ++ " examples total"
>   | otherwise = do
>       example <- readProcess executable [] ""
>       report <- checkExample executable modu prevReport example
>       let newCount = count + 1
>       if newCount `mod` 1000 == 0
>         then putStrLn $ "Tried " ++ show newCount ++ " examples so far"
>         else return ()
>       findExamples executable modu report newCount

Remove all temporary files that are created.

> cleanup :: FilePath -> FilePath -> IO ()
> cleanup file modu = do
>   removeAll modu
>   removeAll file
>   removeDirIfExists ".hpc"
>
> removeAll :: FilePath -> IO ()
> removeAll file = do
>   let program = takeBaseName file
>   removeIfExists program
>   removeIfExists $ addExtension program "hs"
>   removeIfExists $ addExtension program "dyn_hi"
>   removeIfExists $ addExtension program "hi"
>   removeIfExists $ addExtension program "dyn_o"
>   removeIfExists $ addExtension program "o"
>   removeIfExists $ addExtension program "tix"

> removeIfExists :: FilePath -> IO ()
> removeIfExists file = removeFile file `catch` handleExists
>   where handleExists e
>           | isDoesNotExistError e = return ()
>           | otherwise = throwIO e

> removeDirIfExists :: FilePath -> IO ()
> removeDirIfExists dir = removeDirectoryRecursive dir `catch` handleExists
>   where handleExists e
>           | isDoesNotExistError e = return ()
>           | otherwise = throwIO e
