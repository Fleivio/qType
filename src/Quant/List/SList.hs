module List.SList
  ( SList(..), sListToList,  sListCountTo, Length, CountTo, Select, Eval, type (!!),
   ValidSelector
  ) where

import           Data.Kind
import Data.Proxy
import           Fcf                hiding (type (+), Length, type (-),
                                     type (<=))
import           GHC.TypeLits
import Unsafe.Coerce

type SList :: [Natural] -> Type
data SList acs where
  SNil  :: SList '[]
  (:-) :: SNat a -> SList acs -> SList (a ': acs)
infixr 5 :-

instance Show (SList acs) where
  show sl = "#" <> show (sListToList sl)

sListToList :: SList acs -> [Int]
sListToList = unsafeCoerce

sListCountTo :: forall n. SNat n -> SList (CountTo n)
sListCountTo SNat = unsafeCoerce [1..natVal (Proxy @n)]

type family CountTo (n :: Natural) :: [Natural]
  where
    CountTo 0 = '[]
    CountTo n = Eval (CountTo (n - 1) ++ '[n])

type family Length (as :: [a]) :: Natural
 where
  Length '[]      = 0
  Length (a : as) = 1 + Length as

type Select :: [Natural] -> [s] -> [s]
type family Select acs ns where
  Select '[] ns = '[]
  Select (x ': xs) ns = (ns !! x) ': Select xs ns

-- index starts from 0
type (!!) :: [s] -> Natural -> s 
type family xs !! n where 
  '[] !! n = TypeError (Text "Index out of bounds")
  xs !! 0 = TypeError (Text "Zero index selection is not allowed")
  (x ': xs) !! 1 = x
  (x ': xs) !! n = xs !! (n - 1)

type Maximum :: [Natural] -> Natural
type family Maximum a
 where
  Maximum '[]       = TypeError (Text "Unable to Eval Maximum of a empty list")
  Maximum (x : '[]) = x
  Maximum (x : xs)  = If (x <=? Maximum xs) (Maximum xs) x

type Elem :: Natural -> [Natural] -> Bool
type family Elem a as
 where
  Elem a '[]      = 'False
  Elem a (a ': as) = 'True
  Elem a (b ': as) = Elem a as

type HasRepetition :: [Natural] -> Bool
type family HasRepetition xs
 where
  HasRepetition '[] = 'False
  HasRepetition (x ': xs) = If (Elem x xs) 'True (HasRepetition xs)

type HasZero :: [Natural] -> Bool
type family HasZero n 
  where 
    HasZero '[] = 'False
    HasZero (0 ': xs) = 'True
    HasZero (x ': xs) = HasZero xs

------------------------------------------------------------------

type BoundCheck (n :: Natural) (xs :: [Natural]) 
  = If (Maximum xs <=? n) (() :: Constraint) 
    (TypeError (
        Text "Index out of bounds on Qubit selection" 
        :$$: 
        Text "You got " :<>: ShowType n :<>: Text " qubits" :$$: Text "But tried to select qubits " :<>: ShowType xs
        ))

type NoCloningCheck (xs :: [Natural]) 
  = If (HasRepetition xs)
    (TypeError (
        Text "No Cloning Theorem Violation" 
        :$$: 
        Text "You tried to select qubits with repetition " :<>: ShowType xs
        ))
    (() :: Constraint) 

type NoZeroCheck (xs :: [Natural]) 
  = If (HasZero xs)
    (TypeError (
        Text "Zero qubit selection is not allowed" 
        :$$:
        Text "The qubit selection list starts from 1"
        ))
    (() :: Constraint)

type ValidSelectorByLength :: [Natural] -> Natural -> Constraint
type family ValidSelectorByLength nacs size where 
  ValidSelectorByLength nacs size = (BoundCheck size nacs, NoCloningCheck nacs, NoZeroCheck nacs)

type ValidSelector :: [Natural] -> [Natural] -> Constraint
type family ValidSelector nacs acs where 
  ValidSelector nacs acs = ValidSelectorByLength nacs (Length acs)


