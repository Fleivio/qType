module Value.Basis
  ( Basis(..)
  , module Value.List
  ) where

import           Value.List

class Ord a =>
      Basis a
  where
  basis :: [a]

instance Basis Bool
 where
  basis = [True, False]

instance (Basis a, Basis (NList a (s - 1)), KnownNat s) => Basis (NList a s) where 
  -- we could proof that (s + 1 - 1) ~ s, but is too much work, so we just use unsafeCoerce
  basis = [unsafeCoerce $ a :> as | a <- basis @a, as <- basis @(NList a (s - 1))]

instance {-# OVERLAPPING #-} Basis a => Basis (NList a 0)
 where
  basis = [NNil]
