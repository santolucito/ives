> module Ives.ExampleGen.Report (Report(..), genReport, defaultReport, isCovered, hasImproved) where

> import Ives.ExampleGen.HpcReflect
> import Trace.Hpc.Tix
> import System.FilePath
> import System.Process

Represents an HPC report.

> data Report = Report { coveredTop :: Int
>                      , totalTop :: Int
>                      , coveredAlt :: Int
>                      , totalAlt :: Int
>                      } deriving (Show)

> defaultReport :: Report
> defaultReport = Report 0 1 0 1

Generate an HPC report for a program.

> genReport :: String -> IO Report
> genReport moduleName = do
>   tix <- examineTix
>   let tixFile = addExtension moduleName "tix"
>   writeTix tixFile tix
>   report <- readProcess "hpc" ["report", "--include="++moduleName, tixFile] ""
>   let reportLines = lines report
>   let tops = reportLines!!7
>   let coveredTop = read $ takeWhile (/= '/') $ drop 1 $ dropWhile (/= '(') tops
>   let totalTop = read $ takeWhile (/= ')') $ drop 1 $ dropWhile (/= '/') tops
>   let alternatives = reportLines!!5
>   let coveredAlt = read $ takeWhile (/= '/') $ drop 1 $ dropWhile (/= '(') alternatives
>   let totalAlt = read $ takeWhile (/= ')') $ drop 1 $ dropWhile (/= '/') alternatives
>   return $ Report coveredTop totalTop coveredAlt totalAlt

Checks if a report has full coverage.
Might make sense to consider a certain percent coverage as being "covered" or to let a few unexplored branches to slide if it's taking too long.

> isCovered :: Report -> Bool
> isCovered (Report ct tt ca ta) = ca == ta && ct == tt

Checks if the second report is an improvement over the first.

> hasImproved :: Report -> Report -> Bool
> hasImproved (Report ctPrev _ caPrev _) (Report ct tt ca ta) =
>   ca > caPrev || ct > ctPrev || isCovered (Report ct tt caPrev ta)

