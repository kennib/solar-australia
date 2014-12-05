> {-# LANGUAGE EmptyDataDecls, FlexibleInstances #-}

> module Numeric.Units.Dimensional.Currency where

> import Numeric.Units.Dimensional.Prelude
> import Numeric.Units.Dimensional.SIUnits (mega, giga)
> import Numeric.Units.Dimensional.Extensible
> import Numeric.Units.Dimensional (Dimensional (Dimensional), dimUnit)
> import Numeric.NumType (NumType, Zero, Pos1, Neg1)

= Setting up the problem domain =

> data TCurrency  -- Type tag.
> type DCurrency  = DExt TCurrency Pos1 DOne
> type Currency   = Quantity DCurrency

Define show instances. Here we will assume that all currencies will deal
with USD as a baseline and results will be shown as such.

> instance (NumType n, Show d) => Show (DExt TCurrency n d) where
>   show = showDExt "$"

Finally the base units.

> usdollar  :: Num a => Unit DCurrency a
> usdollar  = Dimensional 1

Some combinators for common numerical values.

> mn, bn :: Num a => Unit d a -> Unit d a
> mn = mega
> bn = giga
