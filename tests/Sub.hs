module Main where

import System.Process
import System.IO
import Data.String.Utils
import System.Exit
import Ives.ExampleGen.Utils
import Control.Exception

main :: IO ()
main = do
  removeIfExists "M.tix"
  putStrLn "Compiling example generation program..."
  (e, out, err) <- readProcessWithExitCode "ghc" ["-fhpc", "M.hs"] ""
  case e of
    ExitSuccess -> putStrLn "Compilation successful"
    ExitFailure _ -> error $ "Compilation failed: " ++ err
  (_, Just hout, _, ph) <- createProcess (shell "./M")
                           { std_out = CreatePipe
                           , create_group = True }
  hSetBinaryMode hout False
  hSetBuffering hout LineBuffering
  catch (loop hout ph)
    (\e -> do
         if e == UserInterrupt
           then do
           interruptProcessGroupOf ph
           waitForProcess ph
           return ()
           else return ()
         throwIO e)
  
loop :: Handle -> ProcessHandle -> IO ()
loop h ph = do
  line <- hGetLine h
  if any (`startswith` line) ["EXAMPLES: ", "CHANGE: ", "ERROR: "]
    then do
    putStrLn line
    if startswith "CHANGE: " line
      then do
      interruptProcessGroupOf ph
      waitForProcess ph
      main
      else loop h ph
    else loop h ph
  loop h ph
