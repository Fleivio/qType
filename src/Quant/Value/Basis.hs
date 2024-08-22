module Value.Basis
  ( Basis(..), Bin(..)
  , module Value.List
  ) where

import           Value.List

class Ord a =>
      Basis a
  where
  basis :: [a]

data Bin = O | I
  deriving (Eq, Ord)

instance Show Bin where
  show O = "0"
  show I = "1"

instance Basis Bool
 where
  basis = [True, False]

instance Basis Bin
 where
  basis = [O, I]


instance (Basis a, Basis (NList a (s - 1)), KnownNat s) => Basis (NList a s) where 
  -- we could proof that (s + 1 - 1) ~ s, but is too much work, so we just use unsafeCoerce
  basis = [unsafeCoerce $ a :> as | a <- basis @a, as <- basis @(NList a (s - 1))]

instance {-# OVERLAPPING #-} Basis a => Basis (NList a 0)
 where
  basis = [NNil]
