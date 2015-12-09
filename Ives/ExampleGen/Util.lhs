> module Ives.ExampleGen.Util (createModule, checkType, cleanup) where

> import Language.Haskell.Exts
> import System.IO.Temp
> import System.IO
> import System.Directory
> import System.FilePath
> import System.Process
> import System.IO.Error
> import System.Exit
> import Control.Exception

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
>   (temp, h) <- openTempFile dir (addExtension "checkType" "hs")
>
>   hPutStrLn h $ "import Data.Typeable\n\
>                 \import " ++ moduleName ++ "\n\
>                 \main :: IO ()\n\
>                 \main = putStr $ show $ typeOf " ++ func ++ "\n"
>   hClose h
>   out <- readProcess "runhaskell" [temp] ""
>   removeIfExists temp
>   return $ ty == out

Remove all temporary files that are created.

> cleanup :: String -> IO ()
> cleanup mod = do
>   removeIfExists $ addExtension mod "hs"
>   removeIfExists $ addExtension mod "hi"
>   removeIfExists $ addExtension mod "o"
>   removeIfExists $ addExtension mod "tix"
>   removeIfExists $ joinPath [".hpc", addExtension mod "mix"]

> removeIfExists :: FilePath -> IO ()
> removeIfExists file = removeFile file `catch` handleExists
>   where handleExists e
>           | isDoesNotExistError e = return ()
>           | otherwise = throwIO e

