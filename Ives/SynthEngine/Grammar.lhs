> {-# LANGUAGE DeriveDataTypeable #-}

> module Grammar where

> import Data.Data --TH uses this, but i dont know how to use it yet

this is the grammar for haskell types.
I'm not sure this is the right way to do this.
Seems it should be a lot easier.
Use Happy or parsec libraries?

The most complicated thing we need to parse is 
f :: Con a, Con' b => (a -> a) -> b -> T a b
this grammar/parser has to be written somewhere already
really mgiht be easier to just do it again tho, oh well, good practice


Taking the grammar of type from template haskell. leave out Kinds for now, itll be ok for a while

> data Type = Context Cxt [TyVarBndr] Type  -- ^ @ <ctxt> <var> => <type>@
>          | VarT Name                     -- ^ @a@
>          | ConT Name                     -- ^ @T@
>          | TupleT Int                    -- ^ @(,), (,,), etc.@
>          | ArrowT                        -- ^ @->@
>          | ListT                         -- ^ @[]@
>          | AppT Type Type                -- ^ @T a b@
>          -- | SigT Type Kind                -- ^ @t :: k@
>      deriving( Show, Eq, Data, Typeable )


> type Cxt = [Pred]                 -- ^ @(Eq a, Ord b)@

> data Pred = ClassP Name [Type]    -- ^ @Eq (Int, a)@
>          | EqualP Type Type      -- ^ @F a ~ Bool@
>          deriving( Show, Eq, Data, Typeable )

Again, we will leave out kinds for now

> data TyVarBndr = PlainTV  Name            -- ^ @a@
>               -- | KindedTV Name Kind       -- ^ @(a :: k)@
>      deriving( Show, Eq, Data, Typeable )

Names should be unique, and TH does a lot to ensure this. I'm fairly sure we will be ok without all that

> type Name = String
