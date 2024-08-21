module Value.Operator
  ( mkQop
  , appQop
  , getOpProb
  , Qop(..)
  , Qop'
  , module Value.Value
  , mkCQop
  , mkQop'
  ) where

import           Data.List
import           Data.Map    as Map
import           Value.Value

newtype Qop a s t =
  Qop (Map (NList a s, NList a t) PA)

type Qop' a s = Qop a s s

instance Show a => Show (Qop a s t) where
  show (Qop qop) =
    intercalate "\n" $ do
      ((a, b), pa) <- toList qop
      return $ show pa ++ "|" ++ show a ++ "⟩⟨" ++ show b ++ "|"

getOpProb :: Ord a => Qop a s t -> (NList a s, NList a t) -> PA
getOpProb (Qop qmap) index = Map.findWithDefault 0 index qmap

mkQop :: Ord a => [((NList a s, NList a t), PA)] -> Qop a s t
mkQop = Qop . fromList

mkQop' :: Ord a => [((NList a s, NList a s), PA)] -> Qop' a s
mkQop' = Qop . fromList

mkCQop ::
     forall a s t. (Ord a, Basis (NList a s), Basis (NList a t))
  => (NList a s -> Bool)
  -> Qop a t t
  -> Qop a (s + t) (s + t)
mkCQop enable qop = mkQop $ unchangeCase ++ changeCase
  where
    unchangeCase =
      [ ((a <+> b, a <+> b), 1)
      | a <- basis
      , not (enable a)
      , b <- basis @(NList a t)
      ]
    changeCase =
      [ ((a <+> b1, a <+> b2), getOpProb qop (b1, b2))
      | a <- basis
      , enable a
      , b1 <- basis
      , b2 <- basis
      ]

appQop ::
     (Ord a, Basis (NList a s), Basis (NList a t))
  => Qop a s t
  -> QV a s
  -> QV a t
appQop qop qv = mkQV [(b, prob b) | b <- basis]
  where
    prob b = sum [qop `getOpProb` (a, b) * qv `getProb` a | a <- basis]
