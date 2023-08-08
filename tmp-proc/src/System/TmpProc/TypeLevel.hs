{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Copyright   : (c) 2020-2021 Tim Emiola
SPDX-License-Identifier: BSD3
Maintainer  : Tim Emiola <adetokunbo@users.noreply.github.com>

Defines type-level data structures and combinators used by
"System.TmpProc.Docker" and "System.TmpProc.Warp".

'HList' implements a heterogenous list used to define types that represent
multiple concurrent @tmp procs@.

'KV' is intended for internal use within the @tmp-proc@ package. It allows
indexing and sorting of lists of tmp procs.
-}
module System.TmpProc.TypeLevel
  ( -- * Heterogenous List
    HList (..)
  , (&:)
  , hHead
  , hOf
  , ReorderH (..)

    -- * A type-level Key-Value
  , KV (..)
  , select
  , selectMany
  , LookupKV (..)
  , MemberKV (..)
  , ManyMemberKV (..)

    -- * Other combinators
  , IsAbsent
  , IsInProof

    -- * Re-exports
  , module System.TmpProc.TypeLevel.Sort
  )
where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.Exts (Constraint)
import GHC.TypeLits
  ( ErrorMessage (..)
  , Symbol
  , TypeError
  )
import qualified GHC.TypeLits as TL
import System.TmpProc.TypeLevel.Sort


{- $setup
>>> import Data.Proxy
>>> :set -XDataKinds
>>> :set -XTypeApplications
-}


-- | Obtain the first element of a 'HList'.
hHead :: HList (a ': as) -> a
hHead (x `HCons` _) = x


-- | Get an item in an 'HList' given its type.
hOf :: forall y xs. IsInProof y xs => Proxy y -> HList xs -> y
hOf proxy = go proxy provedIsIn
  where
    go :: Proxy x -> IsIn x ys -> HList ys -> x
    go _ IsHead (y `HCons` _) = y
    go pxy (IsInTail cons) (_ `HCons` rest) = go pxy cons rest


-- | Defines a Heterogenous list.
data HList :: [Type] -> Type where
  HNil :: HList '[]
  HCons :: anyTy -> HList manyTys -> HList (anyTy ': manyTys)


infixr 5 `HCons`


infixr 5 &:


-- | An infix alias for 'HCons'.
(&:) :: x -> HList xs -> HList (x ': xs)
(&:) = HCons


instance Show (HList '[]) where
  show HNil = "HNil"


instance (Show x, Show (HList xs)) => Show (HList (x ': xs)) where
  show (HCons x xs) = show x ++ " &: " ++ show xs


instance Eq (HList '[]) where
  HNil == HNil = True


instance (Eq x, Eq (HList xs)) => Eq (HList (x ': xs)) where
  (HCons x xs) == (HCons y ys) = x == y && xs == ys


-- | Use a type-level symbol as /key/ type that indexes a /value/ type.
data KV :: Symbol -> Type -> Type where
  V :: a -> KV s a


-- | A constraint that confirms that a type is not present in a type-level list.
type family IsAbsent e r :: Constraint where
  IsAbsent e '[] = ()
  IsAbsent e (e ': _) = TypeError (NotAbsentErr e)
  IsAbsent e (e' ': tail) = IsAbsent e tail


type NotAbsentErr e =
  ('TL.Text " type " ':<>: 'TL.ShowType e)
    ':<>: 'TL.Text " is already in this type list, and is not allowed again"


-- | Proves a symbol and its type occur as entry in a list of @'KV'@ types.
data LookupKV (k :: Symbol) t (xs :: [Type]) where
  AtHead :: LookupKV k t (KV k t ': kvs)
  OtherKeys :: LookupKV k t kvs -> LookupKV k t (KV ok ot ': kvs)


-- | Generate proof instances of 'LookupKV'.
class MemberKV (k :: Symbol) (t :: Type) (xs :: [Type]) where
  lookupProof :: LookupKV k t xs


instance {-# OVERLAPPING #-} MemberKV k t '[KV k t] where
  lookupProof = AtHead @k @t @'[]


instance {-# OVERLAPPING #-} MemberKV k t (KV k t ': kvs) where
  lookupProof = AtHead @k @t @kvs


instance MemberKV k t kvs => MemberKV k t (KV ok ot ': kvs) where
  lookupProof = OtherKeys lookupProof


{- | Select an item from an 'HList' of @'KV's@ by /key/.


/N.B/ Returns the first item. It assumes the keys in the KV HList are unique.
/TODO:/ enforce this rule using a constraint.


==== __Examples__


>>> select @"d" @Double  @'[KV "b" Bool, KV "d" Double] (V True &:  V (3.1 :: Double) &: HNil)
3.1
-}
select ::
  forall k t xs.
  MemberKV k t xs =>
  HList xs ->
  t
select = go $ lookupProof @k @t @xs
  where
    go :: LookupKV k1 t1 xs1 -> HList xs1 -> t1
    go AtHead (V x `HCons` _) = x
    go (OtherKeys cons) (_ `HCons` y) = go cons y


{- | Proves that symbols with corresponding types occur as a 'KV' in a
  list of 'KV' types

/Note/ - both the list symbols and @'KV'@ types need to be sorted, with @'KV'@
types sorted by key. /TODO:/ is there an easy way to incorporate this rule into
the proof ?
-}
data LookupMany (keys :: [Symbol]) (t :: [Type]) (xs :: [Type]) where
  FirstOfMany :: LookupMany (k ': '[]) (t ': '[]) (KV k t ': kvs)
  NextOfMany ::
    LookupMany ks ts kvs ->
    LookupMany (k ': ks) (t ': ts) (KV k t ': kvs)
  ManyOthers :: LookupMany ks ts kvs -> LookupMany ks ts (KV ok ot ': kvs)


-- | Generate proof instances of 'LookupMany'.
class ManyMemberKV (ks :: [Symbol]) (ts :: [Type]) (kvs :: [Type]) where
  manyProof :: LookupMany ks ts kvs


instance {-# OVERLAPPING #-} ManyMemberKV '[k] '[t] (KV k t ': ks) where
  manyProof = FirstOfMany @k @t @ks


instance {-# OVERLAPPING #-} ManyMemberKV ks ts kvs => ManyMemberKV (k ': ks) (t ': ts) (KV k t ': kvs) where
  manyProof = NextOfMany manyProof


instance ManyMemberKV ks ts kvs => ManyMemberKV ks ts (KV ok ot ': kvs) where
  manyProof = ManyOthers manyProof


{- | Select items with specified keys from an @'HList'@ of @'KV's@ by /key/.

/N.B./ this this is an internal function.

The keys must be provided in the same order as they occur in the
HList, any other order will likely result in an compiler error.

==== __Examples__


>>> selectMany @'["b"] @'[Bool] @'[KV "b" Bool, KV "d" Double] (V True &:  V (3.1 :: Double) &: HNil)
True &: HNil
-}
selectMany ::
  forall ks ts xs.
  ManyMemberKV ks ts xs =>
  HList xs ->
  HList ts
selectMany = go $ manyProof @ks @ts @xs
  where
    go :: LookupMany ks1 ts1 xs1 -> HList xs1 -> HList ts1
    go FirstOfMany (V x `HCons` _) = x `HCons` HNil
    go (NextOfMany cons) (V x `HCons` y) = x `HCons` go cons y
    go (ManyOthers cons) (_ `HCons` y) = go cons y


{- | Allows reordering of similar @'HList's@.

==== __Examples__


>>> hReorder @_ @'[Bool, Int] ('c' &: (3 :: Int) &: True &: (3.1 :: Double) &: HNil)
True &: 3 &: HNil

>>> hReorder @_ @'[Double, Bool, Int] ('c' &: (3 :: Int) &: True &: (3.1 :: Double) &: HNil)
3.1 &: True &: 3 &: HNil
-}
class ReorderH xs ys where
  hReorder :: HList xs -> HList ys


instance ReorderH xs '[] where
  hReorder _ = HNil


instance (IsInProof y xs, ReorderH xs ys) => ReorderH xs (y ': ys) where
  hReorder xs = hOf @y Proxy xs `HCons` hReorder xs


-- | Proves a type is present in a list of other types.
data IsIn t (xs :: [Type]) where
  IsHead :: IsIn t (t ': tys)
  IsInTail :: IsIn t tys -> IsIn t (otherTy ': tys)


-- | Generate proof instances of 'IsIn'.
class IsInProof t (tys :: [Type]) where
  provedIsIn :: IsIn t tys


instance {-# OVERLAPPING #-} IsInProof t (t ': tys) where
  provedIsIn = IsHead @t @tys


instance IsInProof t tys => IsInProof t (a : tys) where
  provedIsIn = IsInTail provedIsIn
