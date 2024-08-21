module Value.Value
  ( QV
  , getProb
  , (&*)
  , mkQV
  , showQV
  , norm
  , normalize
  , module Value.PA
  , module Value.Basis
  ) where

import           Value.Basis
import           Value.PA

import           Data.List
import           Data.Map    as Map

type QV a s = Map (NList a s) PA

getProb :: Basis (NList a s) => QV a s -> NList a s -> PA
getProb qmap index = Map.findWithDefault 0 index qmap

(&*) ::
     forall a s t.
     ( Basis (NList a s)
     , Basis (NList a t)
     , Basis (NList a (s + t))
     )
  => QV a s
  -> QV a t
  -> QV a (s + t)
qmap1 &* qmap2 =
  mkQV
    [ (x <+> y, getProb qmap1 x * getProb qmap2 y)
    | x <- basis @(NList a s)
    , y <- basis @(NList a t)
    ]

mkQV :: Basis (NList a s) => [(NList a s, PA)] -> QV a s
mkQV = normalize . Map.fromList . Prelude.filter ((/= 0) . snd)

showQV :: Show a => QV a s -> String
showQV qv =
  intercalate " + " $ do
    (a, pa) <- toList qv
    return
      $ case pa of
          0 -> mempty
          _ -> showPAMultiplicative pa ++ "|" ++ show a ++ "⟩"

norm :: QV a s -> Double
norm v = sqrt . sum $ probs
  where
    probs = squareModulus . snd <$> toList v

normalize :: QV a s -> QV a s
normalize qval = (c *) `Map.map` qval
  where
    c = 1 / norm qval :+ 0
