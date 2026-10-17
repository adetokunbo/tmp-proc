{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Copyright   : (c) 2020-2021 Tim Emiola
SPDX-License-Identifier: BSD3
Maintainer  : Tim Emiola <adetokunbo@users.noreply.github.com>

Defines type-level combinators for performing a merge sort of type-level lists.

'SortSymbols' sorts type-level lists of @Symbols@.

The other exported combinators make it easy to implement type-level merge sort
for similar type-level lists.

This is an internal module that provides type-level functions used in
various constraints in "System.TmpProc.Docker".
-}
module System.TmpProc.TypeLevel.Sort
  ( -- * Merge sort for @Symbols@.
    SortSymbols

    -- * Sort combinators
  , Take
  , Drop
  , LengthOf
  , HalfOf
  )
where

import GHC.TypeLits
  ( CmpNat
  , CmpSymbol
  , Nat
  , Symbol
  , type (*)
  , type (+)
  , type (-)
  )


{- | Takes 1 element at a time from a list until the desired length is reached.

For example, @Take \'[\"a\", \"b\", \"c\", \"d\"] 2@ reduces to @\'[\"a\", \"b\"]@.
-}
type family Take (xs :: [k]) (n :: Nat) :: [k] where
  Take '[] n = '[]
  Take xs 0 = '[]
  Take (x ': xs) n = (x ': Take xs (n - 1))


{- | Drops 1 element at a time until the the dropped target is reached.

For example, @Drop \'[\"a\", \"b\", \"c\", \"d\"] 2@ reduces to @\'[\"c\", \"d\"]@,
and @Drop \'[\"a\"] 2@ reduces to @\'[]@.
-}
type family Drop (xs :: [k]) (n :: Nat) :: [k] where
  Drop '[] n = '[]
  Drop xs 0 = xs
  Drop (x ': xs) n = Drop xs (n - 1)


{- | Counts a list, 1 element at a time.

For example, @LengthOf \'[1, 2, 3, 4]@ reduces to @4@.
-}
type family LengthOf (xs :: [k]) :: Nat where
  LengthOf '[] = 0
  LengthOf (x ': xs) = 1 + LengthOf xs


{- | Computes the midpoint of a number.

N.B: maximum value that this works for depends on the reduction limit of the
type-checker.

For example, @HalfOf 99@ reduces to @49@ and @HalfOf 100@ reduces to @50@.
-}
type family HalfOf (n :: Nat) :: Nat where
  -- optimizations for faster compilation
  HalfOf 0 = 0
  HalfOf 1 = 1
  HalfOf 2 = 1
  HalfOf 3 = 1
  HalfOf 4 = 2
  HalfOf 5 = 2
  HalfOf 6 = 3
  HalfOf 7 = 3
  -- the general case
  HalfOf n = HalfOfImpl n 0 n 'LT


-- | Implements 'HalfOf'.
type family HalfOfImpl (n :: Nat) (i :: Nat) (dist :: Nat) (o :: Ordering) :: Nat where
  HalfOfImpl n m dist 'GT = m - 1
  HalfOfImpl n m dist 'EQ = m
  HalfOfImpl n m 1 'LT = m
  HalfOfImpl n m dist 'LT = HalfOfImpl n (m + 2) (n - ((m + 2) * 2)) (CmpNat ((m + 2) * 2) n)


{- | Sort a list of type-level @symbols@ using merge sort.

For example, @SortSymbols \'[\"xyz\", \"def\", \"abc\"]@ reduces to @\'[\"abc\", \"def\", \"xyz\"]@.
-}
type family SortSymbols (xs :: [Symbol]) :: [Symbol] where
  SortSymbols '[] = '[]
  SortSymbols '[x] = '[x]
  SortSymbols '[x, y] = MergeSymbols '[x] '[y] -- an optimization, could be removed
  SortSymbols xs = SortSymbolsStep xs (HalfOf (LengthOf xs))


-- | Used internally by @SortSymbols.
type family SortSymbolsStep (xs :: [Symbol]) (halfLen :: Nat) :: [Symbol] where
  SortSymbolsStep xs halfLen =
    MergeSymbols
      (SortSymbols (Take xs halfLen))
      (SortSymbols (Drop xs halfLen))


-- | Used internally by @SortSymbols.
type family MergeSymbols (xs :: [Symbol]) (ys :: [Symbol]) :: [Symbol] where
  MergeSymbols xs '[] = xs
  MergeSymbols '[] ys = ys
  MergeSymbols (x ': xs) (y ': ys) = MergeSymbolsImpl (x ': xs) (y ': ys) (CmpSymbol x y)


type family MergeSymbolsImpl (xs :: [Symbol]) (ys :: [Symbol]) (o :: Ordering) :: [Symbol] where
  MergeSymbolsImpl (x ': xs) (y ': ys) 'GT = y ': MergeSymbols (x ': xs) ys
  MergeSymbolsImpl (x ': xs) (y ': ys) leq = x ': MergeSymbols xs (y ': ys)
