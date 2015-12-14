This module is used to create and remove temporary files needed during the
example generation process. It also contains functions used by those temporary
files so they don't have to be written out each time.

> module Ives.ExampleGen.Utils (createModule, createProgram, checkType, cleanup, removeIfExists, action, actionFilter) where

> import Language.Haskell.Exts
> import System.IO.Temp
> import System.IO
> import System.Directory
> import System.FilePath
> import System.Process
> import System.IO.Error
> import System.Exit
> import System.FSNotify
> import Control.Exception
> import Control.Concurrent

Creates the program that will actually watch a file for changes and generate
sets of examples for the specified function as the file changes. This program
needs to be generated because it needs to be able to import a dynamically
generated module and use template haskell to make the functions in that
module concrete.

> createProgram :: FilePath -> String -> IO String
> createProgram file func = do
>   dir <- getCurrentDirectory
>   (temp, h) <- openTempFile dir (addExtension "Exampler" "hs")
>   hPutStrLn h $ "{-# LANGUAGE TemplateHaskell #-}                          \n\
>                 \import "++(takeBaseName file)++"                          \n\
>                 \import Ives.ExampleGen.Conc                               \n\
>                 \import Ives.ExampleGen.Gen                                \n\
>                 \import Ives.ExampleGen.Example                            \n\
>                 \import Ives.ExampleGen.Utils                              \n\
>                 \import Test.QuickCheck                                    \n\
>                 \import System.FSNotify                                    \n\
>                 \import System.IO                                          \n\
>                 \import System.FilePath                                    \n\
>                 \import Data.Typeable                                      \n\
>                 \import Control.Concurrent                                 \n\
>                 \                                                          \n\
>                 \main :: IO ()                                             \n\
>                 \main = do                                                 \n\
>                 \  hSetBuffering stdout LineBuffering                      \n\
>                 \  maybeExamples <- tryGetExamples \""++file++"\" \""++func++"\" (show $ typeOf $(concretify '"++func++")) [] :: IO (Maybe ($(concretifyType '"++func++"), [Example]))\n\
>                 \  let examples = case maybeExamples of                    \n\
>                 \        Just (_, examples) -> examples                    \n\
>                 \        Nothing -> []                                     \n\
>                 \  mNotify <- newEmptyMVar                                 \n\
>                 \  manager <- startManager                                 \n\
>                 \  watchDir manager \".\" (actionFilter \""++file++"\") (action mNotify)\n\
>                 \  loop mNotify examples                                   \n\
>                 \                                                          \n\
>                 \loop :: MVar () -> [Example] -> IO ()                     \n\
>                 \loop mNotify prevExamples = do                            \n\
>                 \  takeMVar mNotify                                        \n\
>                 \  maybeExamples <- tryGetExamples \""++file++"\" \""++func++"\" (show $ typeOf $(concretify '"++func++")) prevExamples :: IO (Maybe ($(concretifyType '"++func++"), [Example]))\n\
>                 \  case maybeExamples of                                   \n\
>                 \    Just (_, examples) -> loop mNotify examples           \n\
>                 \    Nothing -> loop mNotify prevExamples                  \n"
>   hClose h
>   return $ takeBaseName temp

Creates a copy of a module that exports the function of interest and then
compiles it with hpc.

> createModule :: FilePath -> String -> IO (String, Maybe String)
> createModule file f = do
>   dir <- getCurrentDirectory
>   (temp, h) <- openTempFile dir file
>   let mod = takeBaseName temp
>
>   res <- parseFile file
>   case res of
>     ParseOk (Module src _ pragmas warning _ imports decls) -> do
>       let nm = ModuleName mod
>       let export = Just [EVar $ UnQual $ Ident f]
>       (hPutStrLn h) . prettyPrint $ Module src nm pragmas warning export imports decls
>       hClose h
>
>       (e, out, err) <- readProcessWithExitCode "ghc" ["-fhpc", temp] ""
>       case e of
>         ExitSuccess -> return (mod, Nothing)
>         ExitFailure _ -> return (mod, Just err)
>     ParseFailed srcLoc err -> return (mod, Just $ show srcLoc ++ ": " ++ err)

Create a program that will print the type of the given function of the given
module and then check that against the given type. Returns true if they match.

> checkType :: String -> String -> String -> IO Bool
> checkType moduleName func ty = do
>   dir <- getCurrentDirectory
>   (temp, h) <- openTempFile dir (addExtension "CheckType" "hs")
>
>   hPutStrLn h $ "{-# LANGUAGE TemplateHaskell #-}\n\
>                 \import Ives.ExampleGen.Conc\n\
>                 \import Test.QuickCheck\n\
>                 \import Data.Typeable\n\
>                 \import " ++ moduleName ++ "\n\
>                 \main :: IO ()\n\
>                 \main = putStr $ show $ typeOf $(concretify '" ++ func ++ ")\n"
>   hClose h
>   out <- readProcess "runhaskell" [temp] ""
>   removeIfExists temp
>   return $ ty == out

Remove all temporary files that are created.

> cleanup :: String -> IO ()
> cleanup mod = do
>   removeIfExists mod
>   removeIfExists $ addExtension mod "hs"
>   removeIfExists $ addExtension mod "hi"
>   removeIfExists $ addExtension mod "dyn_hi"
>   removeIfExists $ addExtension mod "o"
>   removeIfExists $ addExtension mod "dyn_o"
>   removeIfExists $ addExtension mod "tix"
>   removeIfExists $ joinPath [".hpc", addExtension mod "mix"]

Remove a file if it exists.

> removeIfExists :: FilePath -> IO ()
> removeIfExists file = removeFile file `catch` handleExists
>   where handleExists e
>           | isDoesNotExistError e = return ()
>           | otherwise = throwIO e

Action filter and action for fsnotify. Used by the generated program.

> actionFilter :: FilePath -> Event -> Bool
> actionFilter file (Modified path _) = takeFileName path == file
> actionFilter _ _ = False

Just place an empty tuple in the MVar to notify that the file was modified.

> action :: MVar () -> Event -> IO ()
> action mNotify _ = do
>   putMVar mNotify ()

