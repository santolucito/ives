{-# LANGUAGE TemplateHaskell #-}
import Ives.ExampleGen.Gen
import Ives.ExampleGen.Conc
import Ives.ExampleGen.Exampler
import Foo
import DynLoad
import System.INotify
import System.Directory
import System.Process
import GHC hiding (loadModule)
import GHC.Paths (libdir)
import Trace.Hpc.Reflect
import Trace.Hpc.Tix
import Language.Haskell.Exts
import System.FilePath
import System.IO.Temp
import System.IO
import Control.Monad.IO.Class

main :: IO ()
main = do
  execExample
  execExample
  -- inotify <- initINotify
  -- wd <- addWatch inotify [Modify] "Foo.hs" handler
  -- waitForBye

waitForBye :: IO ()
waitForBye = do
  input <- getLine
  if input == "bye" then return () else waitForBye

handler :: Event -> IO ()
handler (Modified _ _) = do
  putStrLn "*** FILE UPDATED ***"
  execExample

execExample :: IO ()
execExample = do
  dir <- getCurrentDirectory
  (temp, h) <- openTempFile dir "Foo.hs"
  let mod = takeBaseName temp
  res <- parseFile "Foo.hs"
  let Module src nm pragmas warning export imports decls = fromParseResult res
  (hPutStrLn h) . prettyPrint $ Module src (ModuleName mod) pragmas warning export imports decls
  hClose h
  out <- readProcess "ghc" ["-fhpc", temp] ""

  f <- getFuncs mod "doh"
  example <- genExample f 10
  print example
  tix <- examineTix
  print tix

getFuncs :: String -> String -> IO $(concretifyType 'doh)
getFuncs mod func = runGhc (Just libdir) $ do
  res <- loadSourceGhc $ addExtension mod "hs"
  case res of
    Just err -> error err
    Nothing  -> do
      f <- execFnGhc mod func
      return f

-- getExample :: IO Example
-- getExample = do
--   let f = $(send 'doh) :: $(concretify 'doh)
--   genExample f 10

-- Keeps generating random examples and printing representative ones until the function is fully covered.

-- findExamples :: FilePath -> String -> Report -> Int -> IO ()
-- findExamples executable modu prevReport count
--   | isCovered prevReport = do
--       putStrLn $ "Tried " ++ show count ++ " examples total"
--   | otherwise = do
--       example <- readProcess executable [] ""
--       report <- checkExample executable modu prevReport example
--       let newCount = count + 1
--       if newCount `mod` 1000 == 0
--         then putStrLn $ "Tried " ++ show newCount ++ " examples so far"
--         else return ()
--       findExamples executable modu report newCount

