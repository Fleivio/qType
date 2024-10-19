module List.NList(NList(..), (<+>), toList) where

import           Data.Kind
import           GHC.TypeLits
import           Unsafe.Coerce

default (Int)

type NList :: Type -> Natural -> Type

data NList a s where
  NNil :: NList a 0
  (:>) :: a -> NList a s -> NList a (s + 1)

infixr 3 :>

instance Show a => Show (NList a s)
 where
  show as = "|" ++ go as ++ "⟩" -- braket notation :D
    where 
      go :: NList a s' -> String
      go NNil      = ""
      go (x :> xs) = show x ++ go xs

instance Eq a => Eq (NList a s)
 where
  NNil == NNil           = True
  (a :> as) == (b :> bs) = a == b && as == unsafeCoerce bs
  _ == _                 = False

instance Ord a => Ord (NList a s)
 where
  compare NNil NNil = EQ
  compare (a :> as) (b :> bs) =
    case compare a b of
      EQ -> compare as $ unsafeCoerce bs
      x  -> x
  compare _ _ = EQ

(<+>) :: NList a s -> NList a t -> NList a (s + t)
NNil <+> a       = a
(a :> as) <+> bs = unsafeCoerce $ a :> (as <+> bs)

toList :: NList a t -> [a]
toList NNil      = []
toList (a :> as) = a : toList as
