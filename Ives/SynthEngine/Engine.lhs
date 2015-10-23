> {-# LANGUAGE OverloadedStrings #-}
> module Ives.SynthEngine.Engine where

> import Language.Haskell.GhcMod
> import Language.Haskell.GhcMod.Monad
> import qualified Data.Text as T
> import Language.Haskell.Exts.Parser

this will grab all the defintions (fxns, types, etc) from Prelude and spit them back as a string. 
I just parse this String to build a grammar of types. There is no way to go from the value level (the String of types) the the type level (Type grammar I am generating)
I think Agda would support this, and I think it is important to have, but I can exactly say why.

> b :: IO ()
> b = do
>   r <- runGmOutT defaultOptions $ runGhcModT (defaultOptions {optDetailed = True}) $ browse "base:Prelude"
>   let tys = map getType (T.lines $ handle r)
>   putStrLn $ show $ head tys
>   let parsedTys = map parseType (map T.unpack tys)
>   putStrLn $ show $ head $ parsedTys

> --handle :: (Either GhcModError a, GhcModLog) -> String
> handle x =
>   case fst x of
>     Left e -> "error"
>     Right t -> T.pack t

> getType :: T.Text -> T.Text
> getType x = last $ T.splitOn "::" x

