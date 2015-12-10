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

> createProgram :: FilePath -> String -> IO String
> createProgram file func = do
>   dir <- getCurrentDirectory
>   (temp, h) <- openTempFile dir (addExtension "Exampler" "hs")
>   let critLine = "maybeExamples <- tryGetExamples \""++file++"\" \""++func++"\" (show $ typeOf $(concretify '"++func++")) [] :: IO (Maybe ($(concretifyType '"++func++"), [Example]))"
>   hPutStrLn h $ "{-# LANGUAGE TemplateHaskell #-}                          \n\
>                 \import "++(takeBaseName file)++"                          \n\
>                 \import Ives.ExampleGen.Conc                               \n\
>                 \import Ives.ExampleGen.Gen                                \n\
>                 \import Ives.ExampleGen.Example                            \n\
>                 \import Ives.ExampleGen.Utils                              \n\
>                 \import System.FSNotify                                    \n\
>                 \import System.IO                                          \n\
>                 \import System.FilePath                                    \n\
>                 \import Data.Typeable                                      \n\
>                 \import Control.Concurrent                                 \n\
>                 \                                                          \n\
>                 \main :: IO ()                                             \n\
>                 \main = do                                                 \n\
>                 \  hSetBuffering stdout LineBuffering                      \n\
>                 \  "++critLine++"                                          \n\
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
>                 \  "++critLine++"                                          \n\
>                 \  case maybeExamples of                                   \n\
>                 \    Just (_, examples) -> loop mNotify examples           \n\
>                 \    Nothing -> loop mNotify prevExamples                  \n"
>   hClose h
>   return $ takeBaseName temp

> createModule :: FilePath -> String -> IO (String, Maybe String)
> createModule file f = do
>   res <- parseFile file
>   let Module src _ pragmas warning _ imports decls = fromParseResult res
>   
>   dir <- getCurrentDirectory
>   (temp, h) <- openTempFile dir file
>   let mod = takeBaseName temp
>   let nm = ModuleName mod
>   let export = Just [EVar $ UnQual $ Ident f]
>   (hPutStrLn h) . prettyPrint $ Module src nm pragmas warning export imports decls
>   hClose h
>
>   (e, out, err) <- readProcessWithExitCode "ghc" ["-fhpc", temp] ""
>   case e of
>     ExitSuccess -> return (mod, Nothing)
>     ExitFailure _ -> return (mod, Just err)

Create a program that will print the type of the given function of the given module.

> checkType :: String -> String -> String -> IO Bool
> checkType moduleName func ty = do
>   dir <- getCurrentDirectory
>   (temp, h) <- openTempFile dir (addExtension "CheckType" "hs")
>
>   hPutStrLn h $ "{-# LANGUAGE TemplateHaskell #-}\n\
>                 \import Ives.ExampleGen.Conc\n\
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

> removeIfExists :: FilePath -> IO ()
> removeIfExists file = removeFile file `catch` handleExists
>   where handleExists e
>           | isDoesNotExistError e = return ()
>           | otherwise = throwIO e

Action filter and action for fsnotify.

> actionFilter :: FilePath -> Event -> Bool
> actionFilter file (Modified path _) = takeFileName path == file
> actionFilter _ _ = False

Just place an empty tuple in the MVar to notify that the file was modified.

> action :: MVar () -> Event -> IO ()
> action mNotify _ = do
>   putMVar mNotify ()

