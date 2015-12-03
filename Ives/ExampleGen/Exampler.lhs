> module Ives.ExampleGen.Exampler (Report, genReport) where

> import Ives.ExampleGen.Gen
> import Trace.Hpc.Reflect
> import Trace.Hpc.Tix
> import System.Directory
> import System.IO
> import System.IO.Error
> import System.FilePath
> import System.Process
> import Control.Exception

Represents an HPC report.

> data Report = Report { coveredAlt :: Int
>                      , totalAlt :: Int
>                      } deriving (Show)

Generate an HPC report for a program.

> genReport :: String -> IO Report
> genReport moduleName = do
>   tix <- examineTix
>   let tixFile = addExtension moduleName "tix"
>   writeTix tixFile tix
>   report <- readProcess "hpc" ["report", "--include="++moduleName, tixFile] ""
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

Remove all temporary files that are created.

> cleanup :: FilePath -> FilePath -> IO ()
> cleanup file moduleName = do
>   removeAll moduleName
>   removeAll file
>   removeDirIfExists ".hpc"

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
