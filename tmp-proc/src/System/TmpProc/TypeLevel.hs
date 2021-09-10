{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-|
Copyright   : (c) 2020-2021 Tim Emiola
SPDX-License-Identifier: BSD3
Maintainer  : Tim Emiola <adetokunbo@users.noreply.github.com >

Defines TypeLevel combinators used by the other System.TmpProc.* modules
-}
module System.TmpProc.TypeLevel
  ( -- * HList
    HList(..)
  , (%:)
  , hHead
  , hSubset

    -- * A Key/Value type where the keys are type-level strings
  , KV(..)
  , select

    -- * Tools for writing constraints on type lists
  , IsAbsent
  , SubsetOf(..)
  , IsSubsetOf(..)
  , LookupGadt(..)
  , MemberGadt(..)
  )
where


import qualified Data.Type.Equality as T
import           GHC.Exts           (Constraint)
import           GHC.TypeLits       (ErrorMessage (..), Symbol, TypeError)
import qualified GHC.TypeLits       as TL


{-| Obtain the first element of a 'HList'. -}
hHead :: HList (a ': as) -> a
hHead (x `HCons` _) = x


{-| Get a subset of the terms in a 'HList'. -}
hSubset :: IsSubsetOf ys xs => HList xs -> HList ys
hSubset = go ssProof
  where
    go :: SubsetOf ys xs -> HList xs -> HList ys
    go SSNil HNil                   = HNil
    go (SSBoth cons) (y `HCons` x)  = y `HCons` go cons x
    go (SSOuter cons) (_ `HCons` x) = go cons x


{-| A Heterogenous list -}
data HList :: [*] -> * where
  HNil  :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

infixr 5 `HCons`
infixr 5 %:
(%:) :: x -> HList xs -> HList (x ': xs)
(%:) = HCons

instance Show (HList '[]) where
  show HNil = "HNil"

instance (Show x, Show (HList xs)) => Show (HList (x ': xs)) where
  show (HCons x xs) = show x ++ " %: " ++ show xs

instance Eq (HList '[]) where
  HNil == HNil = True

instance (Eq x, Eq (HList xs)) => Eq (HList (x ': xs)) where
  (HCons x xs) == (HCons y ys) = x == y && xs == ys


{-| A key, a type-level string that indexes a value, another type  -}
data KV :: Symbol -> * -> * where
  V :: a -> KV s a


{-| Choose between two types. -}
type family If (b :: Bool) tv fv where
  If 'False _ fv  = fv
  If 'True  tv _  = tv


{-| A constraint that confirms type @e@ is not an element of type list @r@. -}
type family IsAbsent e r :: Constraint where
  IsAbsent e '[]           = ()
  IsAbsent e (e' ': tail)  = If (e T.== e') (TypeError (NotAbsentErr e)) (IsAbsent e tail)

type (NotAbsentErr e) =
  ('TL.Text " type " ':<>: 'TL.ShowType e) ':<>:
  ('TL.Text " is already in this type list, and is not allowed again")


data SubsetOf (ys :: [*]) (xs :: [*]) where
  SSNil :: SubsetOf '[] '[]
  SSBoth :: SubsetOf ys xs -> SubsetOf (a : ys) (a : xs)
  SSOuter :: SubsetOf ys xs -> SubsetOf ys (a : xs)


class IsSubsetOf (ys :: [*]) (xs :: [*]) where
  ssProof :: SubsetOf ys xs

instance IsSubsetOf '[] '[] where
  ssProof = SSNil

instance IsSubsetOf ys xs => IsSubsetOf (a : ys) (a : xs) where
  ssProof = SSBoth ssProof

instance IsSubsetOf ys xs => IsSubsetOf ys (a : xs) where
  ssProof = SSOuter ssProof


data LookupGadt (k :: Symbol) t (xs :: [*]) where
  AtHead :: LookupGadt k t (KV k t ': kvs)
  OtherKeys :: LookupGadt k t kvs -> LookupGadt k t (KV ok ot ': kvs)


class MemberGadt (k :: Symbol) (t :: *) (xs :: [*]) where
  lookupProof :: LookupGadt k t xs

instance {-# Overlapping #-} MemberGadt k t '[KV k t] where
  lookupProof = AtHead @k @t @'[]

instance {-# Overlapping #-} MemberGadt k t (KV k t ': kvs) where
  lookupProof = AtHead @k @t @kvs

instance MemberGadt k t kvs => MemberGadt k t (KV ok ot ': kvs) where
  lookupProof = OtherKeys lookupProof


{-| Select an item in a 'HList' of '@'KV's@ by 'key'. -}
select
  :: forall k t xs . MemberGadt k t xs
  => HList xs
  -> t
select = go $ lookupProof @k @t @xs
  where
    go :: LookupGadt k1 t1 xs1 -> HList xs1 -> t1
    go AtHead (V x `HCons` _)         = x
    go (OtherKeys cons) (_ `HCons` y) = go cons y
