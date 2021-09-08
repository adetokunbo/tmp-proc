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
  , KVLookup
  , IsHead

    -- * A Key Value type where the keys are type-level strings
  , KV(..)
  , select

    -- * Detects if a type is/is not in another list of types
  , IsAbsent
  )
where


import qualified Data.Type.Equality as T
import           GHC.Exts           (Constraint)
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


{-| Select an item by 'HList' of '@'KV's@ by 'key'. -}
select
  :: forall s xs . KVMember s xs
  => HList xs
  -> KVLookup s xs
select = select' @s @xs @(KVLookup s xs) @(IsHead s xs)


{-| A constraint that confirms type @e@ is not an element of type list @r@. -}
type family IsAbsent e r :: Constraint where
  IsAbsent e '[]           = ()
  IsAbsent e (e' ': tail)  = If (e T.== e') (TypeError (NotAbsentErr e)) (IsAbsent e tail)

type (NotAbsentErr e) =
  ('TL.Text " type " ':<>: 'TL.ShowType e) ':<>:
  ('TL.Text " is already in this type list, and is not allowed again")
