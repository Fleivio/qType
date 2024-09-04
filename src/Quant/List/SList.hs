module List.SList(SList(..), module List.Key) where

import List.Key
import GHC.TypeLits
import Data.Kind

type SList :: [Natural] -> Type
data SList a where
  SNil  :: SList '[]
  (:>>) :: Key n -> SList a -> SList (n ': a)
infixr 5 :>>
