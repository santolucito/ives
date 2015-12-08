> module Ives.ExampleGen.Report (Report(..), genReport, isCovered, hasImproved) where

> import Trace.Hpc.Reflect
> import Trace.Hpc.Tix
> import System.FilePath
> import System.Process

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

