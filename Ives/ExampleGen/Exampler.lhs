> module Ives.ExampleGen.Exampler (getExamples) where

> import Language.Haskell.Exts
> import System.FilePath
> import System.IO.Temp
> import System.IO
> import System.Directory
> import System.Process
> import Ives.ExampleGen.Gen
> import Ives.ExampleGen.DynLoad
> import Ives.ExampleGen.Report
> import System.IO.Error
> import Control.Exception

> dupMod :: String -> IO String
> dupMod file = do
>   dir <- getCurrentDirectory
>   (temp, h) <- openTempFile dir file
>   let mod = takeBaseName temp
>   res <- parseFile file
>   let Module src nm pragmas warning export imports decls = fromParseResult res
>   (hPutStrLn h) . prettyPrint $ Module src (ModuleName mod) pragmas warning export imports decls
>   hClose h
>   callProcess "ghc" ["-fhpc", temp]
>   return mod

First try old examples then generate new ones until coverage is complete.

> getExamples :: (Exampleable a) => String -> String -> [Example] -> IO (a, [Example])
> getExamples file func prev = do
>   mod <- dupMod file
>   f <- getFunc mod func
>   (report, examples) <- tryExamples f mod (Report 0 1) prev []
>   newExamples <- findExamples f mod report [] 0
>   cleanup mod
>   return (f, examples ++ newExamples)

Tries given examples and returns the ones that improve coverage.

> tryExamples :: (Exampleable a) => a -> String -> Report -> [Example] -> [Example] -> IO (Report, [Example])
> tryExamples f moduleName prevReport (example:examples) keptExamples = do
>   if isCovered prevReport
>     then return (prevReport, keptExamples)
>     else
>       case evalExample f (arguments example) of
>         Just newExample -> do
>           print newExample -- force eval
>           report <- genReport moduleName
>           if hasImproved prevReport report
>             then tryExamples f moduleName report examples (newExample:keptExamples)
>             else tryExamples f moduleName report examples keptExamples
>         Nothing -> tryExamples f moduleName prevReport examples keptExamples
> tryExamples f moduleName prevReport [] keptExamples = return (prevReport, keptExamples)

Keeps generating random examples and printing representative ones until the function is fully covered.

> findExamples :: (Exampleable a) => a -> String -> Report -> [Example] -> Int -> IO [Example]
> findExamples f moduleName prevReport examples count
>   | isCovered prevReport = do
>       putStrLn $ "Tried " ++ show count ++ " examples total"
>       return examples
>   | otherwise = do
>       example <- genExample f 10
>       print example -- force eval
>       let newCount = count + 1
>       if newCount `mod` 1000 == 0
>         then putStrLn $ "Tried " ++ show newCount ++ " examples so far"
>         else return ()
>              
>       report <- genReport moduleName
>       if hasImproved prevReport report
>         then findExamples f moduleName report (example:examples) newCount
>         else findExamples f moduleName report examples newCount

Remove all temporary files that are created.

> cleanup :: String -> IO ()
> cleanup mod = do
>   removeIfExists $ addExtension mod "hs"
>   removeIfExists $ addExtension mod "hi"
>   removeIfExists $ addExtension mod "o"
>   removeIfExists $ addExtension mod "tix"

> removeIfExists :: FilePath -> IO ()
> removeIfExists file = removeFile file `catch` handleExists
>   where handleExists e
>           | isDoesNotExistError e = return ()
>           | otherwise = throwIO e

