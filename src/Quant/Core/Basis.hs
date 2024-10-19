module Core.Basis
  ( Basis(..), Bit(..)
  , module List.List
  , baseFromInt
  ) where

import           List.List
import Data.Proxy

class Ord a =>
      Basis a
  where
  basis :: [a]

data Bit = O | I
  deriving (Eq, Ord)

instance Show Bit where
  show O = "0"
  show I = "1"

instance Basis Bit
 where
  basis = [O, I]

baseFromInt :: forall a. Basis a => Int -> [[a]]
baseFromInt 0 = [[]]
baseFromInt n = do 
  a  <- basis @a -- [O, I]
  as <- baseFromInt @a (n - 1)
  return $ a : as

instance (Basis a, KnownNat s) => Basis (NList a s)
  where
  basis = unsafeCoerce $ baseFromInt @a (fromIntegral $ natVal (Proxy @s))