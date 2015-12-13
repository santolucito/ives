This module generates full sets of representative examples for a given module by looking at hpc reports.

> module Ives.ExampleGen.Gen (exGen, tryGetExamples) where

> import Ives.Types
> import System.FilePath
> import System.IO
> import Ives.ExampleGen.Example
> import Ives.ExampleGen.DynLoad
> import Ives.ExampleGen.Report
> import Ives.ExampleGen.Utils

Placeholder

> exGen :: ExGen
> exGen = id

Given a source file, a function and its type within that file, and a set of
prior examples, tryGetExamples will generate a new set of examples.
It will recycle the examples given to it and keep them if they are still representative.
It will only work if the file compiles and the type signature does not differ from the type specified.

> tryGetExamples :: (Exampleable a) => FilePath -> String -> String -> [Example] -> IO (Maybe (a, [Example]))
> tryGetExamples file func ty prevExamples = do
>   (moduleName, err) <- createModule file func
>   res <- case err of
>     Just errMsg -> do
>       hPutStrLn stderr $ "ERROR: " ++ errMsg
>       return Nothing
>     Nothing -> do
>       same <- checkType moduleName func ty
>       if not same
>         then do
>         putStrLn "CHANGE: Function signature changed"
>         return Nothing
>         else do
>         (f, examples) <- getExamples moduleName func ty prevExamples
>         putStrLn $ "EXAMPLES: " ++ show examples
>         return $ Just (f, examples)
>   cleanup moduleName
>   return res
  
Generates the new set of examples once the module compiles and the function is of the right type.

> getExamples :: (Exampleable a) => String -> String -> String -> [Example] -> IO (a, [Example])
> getExamples moduleName func ty prev = do
>   f <- getFunc moduleName func ty
>   (report, examples) <- tryExamples f moduleName defaultReport prev []
>   newExamples <- findExamples f moduleName report [] 0
>   return (f, examples ++ newExamples)

Tries given examples and returns the ones that improve coverage.
Used to recycle prior examples (same arguments, but possibly new results).

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

Keeps generating random examples for the function until the module is fully covered according to hpc.
It returns the examples that improve the coverage of the module.

> findExamples :: (Exampleable a) => a -> String -> Report -> [Example] -> Int -> IO [Example]
> findExamples f moduleName prevReport examples count
>   | isCovered prevReport = do
>       putStrLn $ "Tried " ++ show count ++ " examples total"
>       return examples
>   | otherwise = do
>       example <- genExample f 1000
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

