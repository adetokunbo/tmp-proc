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

   -- * Confirm membership of an extensible record made of an 'HList' of @'KV's@
  , KVMember
  , Member(..)
  , KVLookup
  , IsHead

    -- * A Key/Value type where the keys are type-level strings
  , KV(..)
  , selectTF

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


{-| Determines if a given symbol belongs to the 'KV' at head of type-level 'List'. -}
type family IsHead (s :: Symbol) (xs :: [*]) :: Bool where
  IsHead s (KV s' _ ': _) = s T.== s'
  IsHead s _              = 'False


{-| KVLookup finds the type corresponding to a symbol in an 'HList' of 'KV'. -}
type family KVLookup (s :: Symbol) (xs :: [*]) :: * where
  KVLookup s '[] = TypeError (('TL.Text "KVLookup:cannot find a KV with key:") ':<>: ('TL.ShowType s))
  KVLookup s (KV s' t ': tail) = If (s T.== s') t (KVLookup s tail)
  KVLookup s (badType ': tail) =
    TypeError (('TL.Text "KVLookup: expected KVs in the type list, instead have: ")
               ':<>:
               ('TL.ShowType badType))


{-| Specifies how obtain a 'KV' by key from a HList of types. -}
class Member (s :: Symbol) xs t (isHead :: Bool) where
  select' :: HList xs -> t

instance Member s (KV s t ': tail) t 'True where
  select' (HCons (V t) _) = t

instance Member s tail t (IsHead s tail)
  => Member s (KV s' t' ': tail) t 'False where
  select' (HCons _ xs) = select' @s @tail @t @(IsHead s tail) xs


{-| Simplifies writing constraint that use 'Member'. -}
type KVMember s xs = Member s xs (KVLookup s xs) (IsHead s xs)


{-| Select an item in a 'HList' of '@'KV's@ by 'key'. -}
selectTF
  :: forall s xs . KVMember s xs
  => HList xs
  -> KVLookup s xs
selectTF = select' @s @xs @(KVLookup s xs) @(IsHead s xs)


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
