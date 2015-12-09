module Main where

import System.Process
import System.IO
import Data.String.Utils

main :: IO ()
main = do
  (_, hout, _, ph) <- (runInteractiveCommand "./M")
  hSetBinaryMode hout False
  hSetBuffering hout LineBuffering
  loop hout
  
loop :: Handle -> IO ()
loop h = do
  line <- hGetLine h
  if startswith "NEW" line then putStrLn line else return ()
  loop h
