> module Ives.ExampleGen.Exampler (findExamples, dupMod, cleanup, removeIfExists) where

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

> findExamples :: (Exampleable a) => String -> String -> IO (a, [Example])
> findExamples file func = do
>   mod <- dupMod file
>   f <- getFunc mod func
>   examples <- findExamplesHelper f mod (Report 0 1) [] 0
>   cleanup mod
>   return (f, examples)

Keeps generating random examples and printing representative ones until the function is fully covered.

> findExamplesHelper :: (Exampleable a) => a -> String -> Report -> [Example] -> Int -> IO [Example]
> findExamplesHelper f moduleName prevReport examples count
>   | isCovered prevReport = do
>       putStrLn $ "Tried " ++ show count ++ " examples total"
>       return examples
>   | otherwise = do
>       example <- genExample f 10
>       print example
>       let newCount = count + 1
>       if newCount `mod` 1000 == 0
>         then putStrLn $ "Tried " ++ show newCount ++ " examples so far"
>         else return ()
> 
>       report <- genReport moduleName
>       if hasImproved prevReport report
>         then findExamplesHelper f moduleName report (example:examples) newCount
>         else findExamplesHelper f moduleName report examples newCount

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

