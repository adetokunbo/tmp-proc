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
Module      : System.TmpProc.TypeLevel
Description : Define TypeLevel combinators used by other modules in this package.
Copyright   : (c)
License     : BSD
Maintainer  : tim@challengehub.com
Stability   : experimental
-}
module System.TmpProc.TypeLevel
  ( -- * HList
    HList(..)
  , (%:)
  , hHead

   -- * Confirm membership of an extensible record made of an 'HList' of @'KV's@
  , KVMember
  , Member(..)
  , Lookup
  , IsHead

    -- * A Key Value type where the keys are type-level strings
  , KV(..)
  , select

    -- * WhenIn items in a 'HList' by their type
  , IsIn(..)
  , WhenIn(..)
  , hIndex
  )
where

import qualified Data.Type.Equality as T
import           GHC.TypeLits       (ErrorMessage (..), Symbol, TypeError)
import qualified GHC.TypeLits       as TL



{-| Obtain the first element of a 'HList'. -}
hHead :: HList (a ': as) -> a
hHead (x `HCons` _) = x


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


{-| WhenIn if the type corresponding to a given symbol in a 'HList' of 'KV'. -}
type family Lookup (s :: Symbol) (xs :: [*]) :: * where
  Lookup s '[] = TypeError (('TL.Text "Cannot find the label: ") ':<>: ('TL.Text s))
  Lookup s (KV s' t ': tail) = If (s T.== s') t (Lookup s tail)
  Lookup s (badType ': tail) =
    TypeError (('TL.Text "Expecting a KV in the type list, instead have: ")
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
type KVMember s xs = Member s xs (Lookup s xs) (IsHead s xs)


{-| Select an item by 'HList' of '@'KV's@ by 'key'. -}
select
  :: forall s xs . KVMember s xs
  => HList xs
  -> Lookup s xs
select = select' @s @xs @(Lookup s xs) @(IsHead s xs)


{-| A proof that @e@ is an element of @r@. -}
data IsIn e r where

  -- | @e@ is located at the head of the list.
  IsHead  :: IsIn e (e ': r)

  -- | @e@ is located somewhere in the tail of the list.
  InTail :: IsIn e r -> IsIn e (e' ': r)


{-| Obtains an 'IsIn'.

An intermediate class used by 'WhenIn'

-}
class WhenIn1 (t :: k) (r :: [k]) (r0 :: [k]) where
  find1Proof :: IsIn t r

instance {-# OVERLAPPING #-} WhenIn1 elem (elem ': rest) orig where
  find1Proof = IsHead

instance WhenIn1 elem items orig => WhenIn1 elem (_head ': items) orig where
  find1Proof = InTail $ find1Proof @_ @elem @items @orig

instance TypeError (WhenIn1NotFound elem orig) => WhenIn1 elem '[] orig where
  find1Proof = error "WhenIn1: TypeError should have caught this"

type WhenIn1NotFound key collection =
  ('TL.Text "WhenIn1: type " ':<>: 'TL.ShowType key) ':<>:
  ('TL.Text " could not be found in list of types " ':<>:
   'TL.ShowType collection)


{-| Obtains an 'IsIn' -}
class WhenIn (t :: k) (r :: [k]) where
  findProof :: IsIn t r

instance WhenIn1 elem items items => WhenIn elem items where
  findProof = find1Proof @_ @elem @items @items


{-| Finds an element of @'HList'@ using 'WhenIn'. -}
hIndex :: WhenIn b as => HList as ->  b
hIndex = go findProof
  where
    go :: IsIn b as -> HList as -> b
    go IsHead (x `HCons` _)      = x
    go (InTail z) (_ `HCons` xs) = go z xs
