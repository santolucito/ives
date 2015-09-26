> {-# LANGUAGE Arrows #-}


> module Ives.Interface.Haskell.Editor (haskEditor) where
> import System.Eval.Haskell
> import Data.Char
> import Data.List
> import Data.List.Split
> import Data.List.Utils
> import System.IO.Unsafe

> import Language.Haskell.Interpreter

> import FRP.UISF  hiding (displayStr)
> import FRP.UISF.UISF
> import FRP.UISF.Graphics.Text

> import Ives.Types

> haskEditor :: ((String,String) -> (String,String)) -> IO ()
> haskEditor exGen = do
>  runUI (defaultUIParams {uiSize=(500, 500)}) (editor exGen)

> editor :: ExGen -> UISF () ()
> editor exGen = setLayout (makeLayout (Stretchy 150) (Fixed 100)) $ 
>                title "Saving Text" $ leftRight $ proc _ -> do
>   code <- topDown $ label "Code: " >>> textField NoWrap "1+?" -< Nothing
>   input <- topDown $ label "Input: " >>> textField NoWrap "1\n2" -< Nothing
>   c <- arr (parse exGen) >>> unique -< (uitextToString code, uitextToString input)
>   output <- uisfPipeE (mapM runCode) -< c
>   o <- (evMap $ arr collect )>>> hold ["none"] -< output
>   topDown $ label "Output: " >>> textField NoWrap "" -< Just $ concat $ intersperse "\n"  o
>   returnA -< ()


> runCode c = 
>   runInterpreter $ setImports ["Prelude"] >> interpret c (as::String)

> collect :: [Either InterpreterError String] -> [String]
> collect (x:xs) =
>   let
>     f x = case x of
>       Left x -> show x 
>       Right x -> x
>   in
>     f x : collect xs
> collect [] = []

> parse :: ExGen -> (String,String) -> [String]
> parse exGen (c,i) = 
>   let
>     is = splitOn ['\n'] i :: [String]
>     all_c = map (replace_line c) is :: [String]
>   in
>     map (\x -> "show (" ++ x ++ ")") all_c


> replace_line :: String -> String -> String
> replace_line code inp = replace "?" inp code 


use existenial types for (as::...) to let us write code for more types
doesn't work since we interpreter works on a very low level


