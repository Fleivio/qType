module List.List
  ( NList(..), (<+>)
  , module Data.Kind
  , module GHC.TypeLits
  , module Unsafe.Coerce
  , module Data.Type.Equality
  , At
  , decompose
  , CountTo
  , Select
  , Length
  , ValidDecomposer
  , ValidSubset
  ) where

import List.NList

import           Data.Kind
import           Data.Proxy
import           Data.Type.Equality
import           Fcf                hiding (Length, type (+), type (-),
                                     type (<=))
import           GHC.TypeLits
import           Unsafe.Coerce

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
    (TypeError
       (Text "No Cloning Theorem Violated" :$$: Text
          "You are trying to clone the index " :<>: ShowType x :<>: Text
          " in the list"))
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
                                       "Index Out of Range on Access of Type Level Lists" :$$: Text
                                       "You Got " :<>: ShowType size :<>: Text
                                       " Qubits" :$$: Text
                                       "And Tried to Access the Index " :<>: ShowType
                                       (Maximum acs)))
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

type family At (as :: [a]) (n :: Natural)
 where
  At (a : as) 1 = a
  At (a : as) n = as `At` (n - 1)

type family Select (acs :: [Natural]) (t :: [a]) :: [a]
 where
  Select '[] t      = '[]
  Select (a : as) t = t `At` a : Select as t

type family ValidSubset (acs1 :: [Natural]) (acs2 :: [Natural]) (t :: Natural) :: Constraint
  where
    ValidSubset acs1 acs2 t 
      = (ValidDecomposer acs2 t, 
        ValidDecomposer acs1 (Length acs2), 
        ValidDecomposer (Select acs1 acs2) t)

