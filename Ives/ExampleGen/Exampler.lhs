This program generates a program that will watch a file for changes and
generate representative examples for the function of interest as the file
changes. It will recompile the generated program upon a type signature change
in the function of interest.

> import System.Process
> import System.Environment
> import System.Directory
> import System.IO
> import System.Exit
> import System.FilePath
> import Data.String.Utils
> import Control.Exception
> import Ives.ExampleGen.Utils

> main :: IO ()
> main = do
>   args <- getArgs
>   if length args /= 2
>     then error "Usage: Exampler <path to module> <function name>"
>     else return ()
>   let file:func:_ = args
>   exists <- doesFileExist file
>   if exists
>     then setCurrentDirectory $ takeDirectory file
>     else error $ file ++ " does not exist"
>   program <- createProgram (takeFileName file) func
> 
>   catch (start program)
>     (\e -> do
>          if e == UserInterrupt
>            then cleanup program
>            else return ()
>          throwIO e)

Compiles the generated program and runs it. It then receives the output of the
generated program via pipes and responds accordingly.

> start :: String -> IO ()
> start program = do
>   removeIfExists $ addExtension program "tix"
>   putStrLn "Compiling example generation program..."
>   (e, out, err) <- readProcessWithExitCode "ghc" ["-fhpc", addExtension program "hs"] ""
>   case e of
>     ExitSuccess -> putStrLn "Compilation successful"
>     ExitFailure _ -> do
>       cleanup program
>       error $ "Compilation failed: " ++ err
>   (_, Just hout, _, ph) <- createProcess (shell $ joinPath [".", program])
>                            { std_out = CreatePipe
>                            , std_err = Inherit
>                            , create_group = True }
>   hSetBinaryMode hout False
>   hSetBuffering hout LineBuffering
> 
>   catch (loop hout ph program)
>     (\e -> do
>          if e == UserInterrupt
>            then do
>            interruptProcessGroupOf ph
>            waitForProcess ph
>            return ()
>            else return ()
>          throwIO e)

Reads lines from the output handle of the program and initiates recompilation
of the program when it discovers a type signature change.

> loop :: Handle -> ProcessHandle -> String -> IO ()
> loop h ph program = do
>   line <- hGetLine h
>   if any (`startswith` line) ["EXAMPLES: ", "CHANGE: "]
>     then do
>     putStrLn line
>     if startswith "CHANGE: " line
>       then do
>       interruptProcessGroupOf ph
>       waitForProcess ph
>       start program
>       else loop h ph program
>     else loop h ph program
