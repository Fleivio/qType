module List.SList
  ( 
    module Data.Kind
  , module GHC.TypeLits
  , module Data.Type.Equality,
  Eval, CountTo, Select, ToListOfInts(..), EVPS, Length, SubsetValid
  ) where

import           Data.Kind
import           Data.Proxy
import           Data.Type.Equality
import           Fcf                hiding (type (+), type (-),
                                     type (<=))
import Fcf.Data.List(Cons)
import           GHC.TypeLits

class ToListOfInts (as :: [Natural]) where
  toListOfInts :: [Int]

instance ToListOfInts '[]
 where
  toListOfInts = []

instance (KnownNat a, ToListOfInts as) => ToListOfInts (a ': as)
 where
  toListOfInts = fromIntegral (natVal (Proxy @a)) : toListOfInts @as

data Maximum :: [Natural] -> Exp Natural

type instance Eval (Maximum xs) = MaximumImpl xs
type family MaximumImpl (a :: [Natural]) :: Natural
 where
  MaximumImpl '[]       = TypeError (Text "Unable to Eval Maximum of a empty list")
  MaximumImpl (x : '[]) = x
  MaximumImpl (x : xs)  = If (x <=? MaximumImpl xs) (MaximumImpl xs) x

data Elem :: Natural -> [Natural] -> Exp Bool
type instance Eval (Elem a as) = ElemImpl a as
type family ElemImpl (a :: Natural) (as :: [Natural]) :: Bool
 where
  ElemImpl a '[]      = 'False
  ElemImpl a (a ': as) = 'True
  ElemImpl a (b ': as) = ElemImpl a as

data HasRepetition :: [Natural] -> Exp Bool
type instance Eval (HasRepetition '[]) = 'False
type instance Eval (HasRepetition (x ': xs)) = If (Eval (Elem x xs)) 'True (Eval (HasRepetition xs))

data HasZero :: [Natural] -> Exp Bool
type instance Eval (HasZero '[]) = 'False
type instance Eval (HasZero (x ': xs)) = If (x == 0) 'True (Eval (HasZero xs))

data CountTo :: Natural -> Exp [Natural]
type instance Eval (CountTo n) = CountToImpl n

type family CountToImpl (n :: Natural) :: [Natural]
 where
  CountToImpl 0 = '[]
  CountToImpl n = Eval (CountToImpl (n - 1) ++ '[ n])

data PowerSet :: [s] -> Exp [[s]]
type instance Eval (PowerSet '[]) = '[ '[]]
type instance Eval (PowerSet (x ': xs)) 
  = Eval ( (++) (Eval (PowerSet xs)) =<< (Map (Cons x) =<< PowerSet xs))

data (!!) :: [s] -> Natural -> Exp s
type instance Eval ('[] !! n) = TypeError (Text "Index out of bounds")
type instance Eval ((x ': xs) !! n) 
  = If (n == 0) x (Eval (xs !! (n - 1)))

data Select :: [s] -> [Natural] -> Exp [s]
type instance Eval (Select '[] ns) = '[]
type instance Eval (Select (x ': xs) ns) = Eval (ns !! x) ': Eval (Select xs ns)

data ValidSelector :: Natural -> [Natural] -> Exp Constraint
type instance Eval (ValidSelector size acs)
  = Eval (Constraints [Eval (Maximum acs) <= size,
                       Eval (HasZero acs) ~ False,
                       Eval (HasRepetition acs) ~ False,
                       ToListOfInts acs])

data ValidPowersetSelector :: [Natural] -> Natural -> Exp Constraint
type instance Eval (ValidPowersetSelector acs size) =
  Eval (Constraints =<< Map (ValidSelector size) (Eval (PowerSet acs)))

-- Evaluated Valid Powerset Selector 
type EVPS acs l = (Eval (ValidPowersetSelector acs l), ToListOfInts acs)
type SubsetValid acs nacs l = (EVPS acs l, EVPS (Eval (Select nacs acs)) l, EVPS nacs (Eval (Length acs)))