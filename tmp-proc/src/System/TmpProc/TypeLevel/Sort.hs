{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoStarIsType          #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-|
Copyright   : (c) 2020-2021 Tim Emiola
SPDX-License-Identifier: BSD3
Maintainer  : Tim Emiola <adetokunbo@users.noreply.github.com >

Defines type combinators that help sort type-level lists.

-}
module System.TmpProc.TypeLevel.Sort
  ( -- * An implementation of merge sort for @Symbols@.
    SymbolSort

     -- * Type families that simplify implementing merge sorts
   , Take
   , Drop
   , LengthOf
   , HalfOf
  )
where

import           GHC.TypeLits (CmpNat, CmpSymbol, Nat, Symbol, type (*),
                               type (+), type (-))


-- $setup
-- >>> import Data.Proxy
-- >>> :set -XDataKinds
-- >>> :set -XTypeFamilies


{-| Take 1 element at a time from a list until the desired length is reached.

>>> :{
_testTake1 :: ( Take '[1, 2, 3, 4] 2 ~ x, x ~ '[1, 2] ) => Proxy x
_testTake1 = Proxy
:}

>>> :t _testTake1
_testTake1 :: Proxy '[1, 2]

-}
type family Take (xs :: [k]) (n :: Nat) :: [k] where
    Take '[] n = '[]
    Take xs 0 = '[]
    Take (x ': xs) n = (x ': Take xs (n - 1))


{-| Drop 1 element at a time until the the dropped target is reached.

>>> :{
_testDrop1 :: ( Drop '[1, 2, 3, 4] 2 ~ x, x ~ '[3, 4] ) => Proxy x
_testDrop1 = Proxy
:}

>>> :t _testDrop1
_testDrop1 :: Proxy '[3, 4]

>>> :{
_testDrop2 :: ( Drop '[1] 2 ~ x, x ~ '[] ) => Proxy x
_testDrop2 = Proxy
:}

>>> :t _testDrop2
_testDrop2 :: Proxy '[]

-}
type family Drop (xs :: [k]) (n :: Nat) :: [k] where
    Drop '[] n = '[]
    Drop xs 0 = xs
    Drop (x ': xs) n = Drop xs (n - 1)


{-| Count the list, 1 element at a time.

>>> :{
_testLengthOf :: ( LengthOf '[1, 2, 3, 4] ~ x, x ~ 4 ) => Proxy x
_testLengthOf = Proxy
:}

>>> :t _testLengthOf
_testLengthOf :: Proxy 4

-}
type family LengthOf (xs :: [k]) :: Nat where
    LengthOf '[] = 0
    LengthOf (x ': xs) = 1 + LengthOf xs


{-| Computes the midpoint of a number.

N.B: maximum value this works for depends on the reduction limit of the type-checker.

>>> :{
_testHalfOf :: ( HalfOf 99 ~ x, x ~ 49 ) => Proxy x
_testHalfOf = Proxy
:}

>>> :t _testHalfOf
_testHalfOf :: Proxy 49

>>> :{
_testHalfOf :: ( HalfOf 100 ~ x, x ~ 50 ) => Proxy x
_testHalfOf = Proxy
:}

>>> :t _testHalfOf
_testHalfOf :: Proxy 50

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

{-| Implements 'HalfOf'. -}
type family HalfOfImpl (n :: Nat) (i :: Nat) (dist :: Nat) (o :: Ordering) :: Nat where
    HalfOfImpl n m dist 'GT = m - 1
    HalfOfImpl n m dist 'EQ = m
    HalfOfImpl n m 1 'LT = m
    HalfOfImpl n m dist 'LT = HalfOfImpl n (m + 2) (n - ((m + 2) * 2)) (CmpNat ((m + 2) * 2) n)


{-| Sort a list of @symbols@ using merge sort.

>>> :{
_testSymSort1 ::
    (SymbolSort '["xyz", "def", "abc"] ~ x
    , x ~ '["abc", "def", "xyz"]
    ) => Proxy x
_testSymSort1 = Proxy
:}

>>> :t _testSymSort1
_testSymSort1 :: Proxy '["abc", "def", "xyz"]

-}
type family SymbolSort (xs :: [Symbol]) :: [Symbol] where
    SymbolSort '[]     = '[]
    SymbolSort '[x]    = '[x]
    SymbolSort '[x, y] = SymbolMerge '[x] '[y] -- an optimization, could be removed
    SymbolSort xs      = SymbolSortStep xs (HalfOf (LengthOf xs))


type family SymbolSortStep (xs :: [Symbol]) (halfLen :: Nat) :: [Symbol] where
    SymbolSortStep xs halfLen = SymbolMerge
      (SymbolSort (Take xs halfLen))
      (SymbolSort (Drop xs halfLen))

type family SymbolMerge (xs :: [Symbol]) (ys :: [Symbol]) :: [Symbol] where
    SymbolMerge xs '[]              = xs
    SymbolMerge '[] ys              = ys
    SymbolMerge (x ': xs) (y ': ys) = SymbolMergeImpl (x ': xs) (y ': ys) (CmpSymbol x y)

type family SymbolMergeImpl (xs :: [Symbol]) (ys :: [Symbol]) (o :: Ordering) :: [Symbol] where
    SymbolMergeImpl (x ': xs) (y ': ys) 'GT = y ': SymbolMerge (x ': xs) ys
    SymbolMergeImpl (x ': xs) (y ': ys) leq = x ': SymbolMerge xs (y ': ys)
