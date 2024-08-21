module Value.List
  ( NList(..)
  , SList(..)
  , module Data.Kind
  , module GHC.TypeLits
  , module Unsafe.Coerce
  , module Data.Type.Equality
  , (<+>)
  , (<!!>)
  , Maximum
  , NoCloning
  , ValidComposer
  , decompose
  , toListOfInts
  ) where

import           Data.Kind
import           Data.Proxy
import           Data.Type.Equality
import           Fcf                hiding (type (+), type (-), type (<=))
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
  show NNil      = "#"
  show (a :> as) = show a ++ ":" ++ show as

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

(<!!>) ::
     forall a s n. (n <= (s - 1))
  => NList a s
  -> SNat n
  -> a
as <!!> SNat = toList as !! (fromIntegral . natVal $ Proxy @n)

toList :: NList a t -> [a]
toList NNil      = []
toList (a :> as) = a : toList as

type SList :: [Natural] -> Type

data SList list where
  SNil :: SList '[]
  (:>>) :: SNat a -> SList as -> SList (a : as)

infixr 4 :>>

toListOfInts :: SList as -> [Int]
toListOfInts SNil = []
toListOfInts ((SNat :: SNat a) :>> as) =
  fromIntegral (natVal (Proxy @a)) : toListOfInts as

type family Maximum (a :: [Natural]) :: Natural
 where
  Maximum '[]       = TypeError (Text "Unable to Eval Maximum of a empty list")
  Maximum (x : '[]) = x
  Maximum (x : xs)  = If (x <=? Maximum xs) (Maximum xs) x

type family Elem (a :: Natural) (as :: [Natural]) :: Bool
 where
  Elem a '[]      = 'False
  Elem a (a : as) = 'True
  Elem a (b : as) = Elem a as

type family NoCloning (a :: [Natural]) :: Constraint
 where
  NoCloning '[]      = ()
  NoCloning (x : xs) = If
    (Elem x xs)
    (TypeError (Text "No Cloning Theorem Violated"))
    (NoCloning xs)

type family ValidDecomposer (accessors :: [Natural]) (size :: Natural) :: Constraint
 where
  ValidDecomposer acs size = ( If
                                 (Maximum acs <=? size)
                                 (() :: Constraint)
                                 (TypeError
                                    (Text
                                       "Index Out of Range on Access of Type Level Lists"))
                             , If
                                 (Elem 0 acs)
                                 (TypeError
                                    (Text
                                       "Invalid Zero Index, Type-Level Lists are indexed starting at 1"))
                                 (() :: Constraint)
                             , NoCloning acs)

type family Lenght (as :: [a]) :: Natural
 where
  Lenght '[]      = 0
  Lenght (a : as) = 1 + Lenght as

type family ValidComposer (accessors :: [Natural]) (size :: Natural) :: Constraint
 where
  ValidComposer acs size = ( If
                               (Maximum acs <=? Lenght acs + size)
                               (() :: Constraint)
                               (TypeError
                                  (Text
                                     "Construction Out of Range on Access of Type Level Lists"))
                           , If
                               (Elem 0 acs)
                               (TypeError
                                  (Text
                                     "Invalid Zero Index, Type-Level Lists are indexed starting at 1"))
                               (() :: Constraint)
                           , NoCloning acs)

decompose ::
     forall acs n a. ValidDecomposer acs n
  => SList acs
  -> NList a n
  -> (NList a (Lenght acs), NList a (n - Lenght acs))
decompose slist nlist = (unsafeCoerce selectionList, unsafeCoerce restList)
  where
    term_level_sList = toListOfInts slist
    term_level_nList = toList nlist
    selectionList =
      flip (!!) . pred <$> term_level_sList <*> pure term_level_nList
    restList =
      map snd
        $ filter ((`notElem` term_level_sList) . fst)
        $ [1 ..] `zip` term_level_nList
