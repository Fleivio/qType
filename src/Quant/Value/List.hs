module Value.List
  ( NList(..)
  , module Data.Kind
  , module GHC.TypeLits
  , module Unsafe.Coerce
  , module Data.Type.Equality
  , (<+>)
  , (<!!>)
  , At
  , decompose
  , compose
  , CountTo
  , Select
  , Length
  , ValidDecomposer
  ) where

import           Data.Kind
import           Data.List          (sortOn)
import           Data.Proxy
import           Data.Type.Equality
import           Fcf                hiding (Length, type (+), type (-),
                                     type (<=))
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

class ToListOfInts (as :: [Natural]) where
  toListOfInts :: [Int]

instance ToListOfInts '[]
 where
  toListOfInts = []

instance (KnownNat a, ToListOfInts as) => ToListOfInts (a : as)
 where
  toListOfInts = fromIntegral (natVal (Proxy @a)) : toListOfInts @as

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

type family NoZero (a :: [Natural]) :: Constraint
 where
  NoZero '[]     = ()
  NoZero (0 : _) = TypeError
    (Text "Invalid Zero Index, Type-Level Lists are indexed starting at 1")
  NoZero (_ : xs) = NoZero xs

type family ValidDecomposer (accessors :: [Natural]) (size :: Natural) :: Constraint
 where
  ValidDecomposer acs size = ( If
                                 (Maximum acs <=? size)
                                 (() :: Constraint)
                                 (TypeError
                                    (Text
                                       "Index Out of Range on Access of Type Level Lists" 
                                       :$$: (Text "You Got ") :<>: (ShowType size) :<>: (Text " Qubits")
                                       :$$: (Text "And Tried to Access the Index") :<>: (ShowType (Maximum acs)))
                                       )
                             , ToListOfInts acs
                             , NoZero acs
                             , NoCloning acs)

type family Length (as :: [a]) :: Natural
 where
  Length '[]      = 0
  Length (a : as) = 1 + Length as



type family (<++>) (as :: [a]) (bs :: [a]) :: [a]
 where
  (<++>) '[] bs      = bs
  (<++>) (a : as) bs = a : (as <++> bs)

type family CountTo (n :: Natural) :: [Natural]
 where
  CountTo 0 = '[]
  CountTo n = CountTo (n - 1) <++> '[ n]

type family ValidComposer (accessors :: [Natural]) (size :: Natural) :: Constraint
 where
  ValidComposer acs size = ( If
                               (Maximum acs <=? Length acs + size)
                               (() :: Constraint)
                               (TypeError
                                  (Text
                                     "Construction Out of Range on Access of Type Level Lists"))
                           , ToListOfInts acs
                           , NoZero acs
                           , NoCloning acs)

decompose' :: [Int] -> [a] -> ([a], [a])
decompose' slist nlist = (selectionList, restList)
  where
    selectionList = flip (!!) . pred <$> slist <*> pure nlist
    restList      = map snd $ filter ((`notElem` slist) . fst) $ [1 ..] `zip` nlist

decompose ::
     forall acs n a. ValidDecomposer acs n
  => NList a n
  -> (NList a (Length acs), NList a (n - Length acs))
decompose nlist = (unsafeCoerce selectionList, unsafeCoerce restList)
  where
    term_level_sList          = toListOfInts @acs
    term_level_nList          = toList nlist
    (selectionList, restList) = decompose' term_level_sList term_level_nList

compose' :: [Int] -> [a] -> [a] -> [a]
compose' slist selectionList restList = updatedList pairs restList
  where
    pairs = sortOn fst $ zip (pred <$> slist) selectionList
    updatedList p acc =
      case p of
        []        -> acc
        (i, x):xs -> updatedList xs $ take i acc ++ [x] ++ drop i acc

compose ::
     forall acs n a. ValidComposer acs n
  => NList a n
  -> NList a (Length acs)
  -> NList a (Length acs + n)
compose nlist selectionList =
  unsafeCoerce
    $ compose' term_level_sList term_level_selectionList term_level_nList
  where
    term_level_sList         = toListOfInts @acs
    term_level_nList         = toList nlist
    term_level_selectionList = toList selectionList

type family At (as :: [a]) (n :: Natural)
 where
  At (a : as) 0 = TypeError
    (Text "Invalid Zero Index, Type-Level Lists are indexed starting at 1")
  At (a : as) 1 = a
  At (a : as) n = as `At` (n - 1)
  At '[] n = TypeError (Text "Index Out of Range on Access of Type Level Lists")

type family Select (acs :: [Natural]) (t :: [a]) :: [a]
 where
  Select '[] t      = '[]
  Select (a : as) t = t `At` a : Select as t