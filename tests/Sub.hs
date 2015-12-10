import System.Process
import System.Environment
import System.Directory
import System.IO
import System.Exit
import System.FilePath
import Data.String.Utils
import Control.Exception
import Ives.ExampleGen.Utils

main :: IO ()
main = do
  args <- getArgs
  if length args /= 2
    then error "Usage: Exampler <path to module> <function name>"
    else return ()
  let file:func:_ = args
  exists <- doesFileExist file
  if exists
    then setCurrentDirectory $ takeDirectory file
    else error $ file ++ " does not exist"
  program <- createProgram (takeFileName file) func

  catch (start program)
    (\e -> do
         if e == UserInterrupt
           then cleanup program
           else return ()
         throwIO e)

start :: String -> IO ()
start program = do
  removeIfExists $ addExtension program "tix"
  putStrLn "Compiling example generation program..."
  (e, out, err) <- readProcessWithExitCode "ghc" ["-fhpc", addExtension program "hs"] ""
  case e of
    ExitSuccess -> putStrLn "Compilation successful"
    ExitFailure _ -> error $ "Compilation failed: " ++ err
  (_, Just hout, _, ph) <- createProcess (shell $ joinPath [".", program])
                           { std_out = CreatePipe
                           , create_group = True }
  hSetBinaryMode hout False
  hSetBuffering hout LineBuffering

  catch (loop hout ph program)
    (\e -> do
         if e == UserInterrupt
           then do
           interruptProcessGroupOf ph
           waitForProcess ph
           return ()
           else return ()
         throwIO e)
  
loop :: Handle -> ProcessHandle -> String -> IO ()
loop h ph program = do
  line <- hGetLine h
  if any (`startswith` line) ["EXAMPLES: ", "CHANGE: ", "ERROR: "]
    then do
    putStrLn line
    if startswith "CHANGE: " line
      then do
      interruptProcessGroupOf ph
      waitForProcess ph
      start program
      else loop h ph program
    else loop h ph program
