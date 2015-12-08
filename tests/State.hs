import System.INotify
import Control.Concurrent

main :: IO ()
main = do
  inotify <- initINotify
  content <- readFile "Foo.hs"
  m <- newEmptyMVar
  wd <- addWatch inotify [Modify] "Foo.hs" (handler m)
  doIt m (length content)


doIt :: MVar Int -> Int -> IO ()
doIt m len = do
  putStrLn $ "old len: " ++ show len
  newLen <- takeMVar m
  putStrLn $ "new len: " ++ show newLen
  doIt m newLen

handler :: MVar Int -> Event -> IO ()
handler m _ = do
  content <- readFile "Foo.hs"
  putMVar m (length content)
  
