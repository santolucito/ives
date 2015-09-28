> module Grammar where

this is the grammar for haskell types.
I'm not sure this is the right way to do this.
Seems it should be a lot easier.
Use Happy or parsec libraries?

The most complicated thing we need to parse is 
f :: Con a, Con' b => (a -> a) -> b -> T a b
this grammar/parser has to be written somewhere already
really mgiht be easier to just do it again tho, oh well, good practice

> data Type
>   = Simple Term -- a -> b
>   | ContextedType Context Term -- Con a => a

> newtype Context
>   = C String Term -- Ord a

> data Term
>   = Lit String -- a
>   | Arrow Term Term -- a -> b


