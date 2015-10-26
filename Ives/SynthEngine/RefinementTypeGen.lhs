> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE ExtendedDefaultRules #-}
> {-# OPTIONS_GHC -fno-warn-type-defaults #-}

> module RefinementTypeGen where

> import Prelude 
> import qualified Shelly as S
> import qualified Data.Text as T

> import Control.Monad

import ExampleGen


Avaible types

assign a refinement type to code
take a file path with the definition of one example
concat that file to all rTypes and see which ones work

> type RType = String

these types should be generalized later to
eqLen, leqLen, geqLen :: Measure a,b => (a,b) -> Bool

> type Template a b = (RType, (([a],[b])->Bool))

> templates :: [Template a b]
> templates = 
>   [ ("{-@ hofxn :: _ -> xs:[a] -> {v:[b] | (len v) =  (len xs)} @-}", f (==))
>   , ("{-@ hofxn :: _ -> xs:[a] -> {v:[b] | (len v) >= (len xs)} @-}", f (>=))
>   , ("{-@ hofxn :: _ -> xs:[a] -> {v:[b] | (len v) <= (len xs)} @-}", f (<=))
>   ]

> f :: (Int -> Int -> Bool) -> ([a],[b]) -> Bool
> f c = (\(x,y) -> c (length x) (length y))

and $ zipWith 

> rTypeAssign :: FilePath -> IO([Template a b])
> rTypeAssign exFile =
>   filterM (test exFile . fst) templates


> -- | Run liquid haskell
> test :: FilePath -> RType -> IO(Bool)
> test f ty = do
>   fcontents <- readFile f
>   let tmp = f++"tmp.hs"
>   writeFile tmp (ty++"\n")
>   appendFile tmp fcontents
>   result <- S.shelly $ S.errExit False $ S.run "liquid" [T.pack tmp]
>   return (isSafe result)

> isSafe :: T.Text -> Bool
> isSafe r = 
>   T.isInfixOf "* SAFE *" r



