{-# LANGUAGE TemplateHaskell #-}
import Foo
import Ives.ExampleGen.Conc
import Ives.ExampleGen.DynLoad
import Ives.ExampleGen.Gen
import Ives.ExampleGen.Exampler
import Trace.Hpc.Reflect

main :: IO ()
main = do
  mod <- dupMod "Foo.hs"
  f <- getFunc mod "doh" :: IO $(concretifyType 'doh)
  example <- genExample f 10
  print example
  cleanup mod
  -- tix <- examineTix
  -- print tix

  mod2 <- dupMod "Foo.hs"
  f2 <- getFunc mod2 "doh" :: IO $(concretifyType 'doh)
  example2 <- genExample f2 10
  print example2
  cleanup mod2
  tix2 <- examineTix
  print tix2

