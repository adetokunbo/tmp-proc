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
  , selectMany
  , LookupKV(..)
  , MemberKV(..)
  , ManyMemberKV(..)

    -- * Tools for writing constraints on type lists
  , IsAbsent
  , SubsetOf(..)
  , IsSubsetOf(..)
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


{-| Generate proof instances of 'SubsetOf'. -}
class IsSubsetOf (ys :: [*]) (xs :: [*]) where
  ssProof :: SubsetOf ys xs

instance IsSubsetOf '[] '[] where
  ssProof = SSNil

instance IsSubsetOf ys xs => IsSubsetOf (a : ys) (a : xs) where
  ssProof = SSBoth ssProof

instance IsSubsetOf ys xs => IsSubsetOf ys (a : xs) where
  ssProof = SSOuter ssProof


{-| Proves a symbols its type occur together as a 'KV' in a list of 'KV' types. -}
data LookupKV (k :: Symbol) t (xs :: [*]) where
  AtHead :: LookupKV k t (KV k t ': kvs)
  OtherKeys :: LookupKV k t kvs -> LookupKV k t (KV ok ot ': kvs)


{-| Generate proof instances of 'LookupKV'. -}
class MemberKV (k :: Symbol) (t :: *) (xs :: [*]) where
  lookupProof :: LookupKV k t xs

instance {-# Overlapping #-} MemberKV k t '[KV k t] where
  lookupProof = AtHead @k @t @'[]

instance {-# Overlapping #-} MemberKV k t (KV k t ': kvs) where
  lookupProof = AtHead @k @t @kvs

instance MemberKV k t kvs => MemberKV k t (KV ok ot ': kvs) where
  lookupProof = OtherKeys lookupProof


{-| Select an item in a 'HList' of '@'KV's@ by 'key'. -}
select
  :: forall k t xs . MemberKV k t xs
  => HList xs
  -> t
select = go $ lookupProof @k @t @xs
  where
    go :: LookupKV k1 t1 xs1 -> HList xs1 -> t1
    go AtHead (V x `HCons` _)         = x
    go (OtherKeys cons) (_ `HCons` y) = go cons y


{-| Proves that some symbols and corresponding types occur together as a 'KV' in a
  list of 'KV' types. -}
data LookupMany (keys :: [Symbol]) (t :: [*]) (xs :: [*]) where
  FirstOfMany :: LookupMany (k ': '[]) (t ': '[]) (KV k t ': kvs)
  NextOfMany
    :: LookupMany ks ts kvs
    -> LookupMany (k ': ks) (t ': ts) (KV k t ': kvs)

  ManyOthers :: LookupMany ks ts kvs -> LookupMany ks ts (KV ok ot ': kvs)


{-| Generate proof instances of 'LookupMany'. -}
class ManyMemberKV (ks :: [Symbol]) (ts :: [*]) (kvs :: [*])  where
  manyProof :: LookupMany ks ts kvs

instance {-# Overlapping #-} ManyMemberKV '[k] '[t] '[KV k t] where
  manyProof = FirstOfMany @k @t @'[]

instance {-# Overlapping #-} ManyMemberKV ks ts kvs => ManyMemberKV (k ': ks) (t ': ts) (KV k t ': kvs) where
  manyProof = NextOfMany manyProof

instance ManyMemberKV ks ts kvs  => ManyMemberKV ks ts (KV ok ot ': kvs) where
  manyProof = ManyOthers manyProof


{-| Select items with specified keys from an 'HList' of '@'KV's@ by 'key'.

Note: there is a known bug; the specified keys have to be provided in the same
order as they are in the HList, any other order will usually result in an
compiler error.

-}
selectMany
  :: forall ks ts xs . ManyMemberKV ks ts xs
  => HList xs
  -> HList ts
selectMany = go $ manyProof @ks @ts @xs
  where
    go :: LookupMany ks1 ts1 xs1 -> HList xs1 -> HList ts1
    go FirstOfMany (V x `HCons` _)        = x `HCons` HNil
    go (NextOfMany cons) (V x `HCons` y)  = x `HCons` go cons y
    go (ManyOthers cons) (_ `HCons` y)    = go cons y
