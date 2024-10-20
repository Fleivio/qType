{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Core.Observation
  ( observeV
  , observeRef
  , observeN
  , module Core.QR
  ) where

import           Core.QR
import Data.Proxy

import           Data.IORef
import           Data.List     (find)
import           System.Random (Random (randomR), getStdRandom)

observeV :: Basis a => KnownNat s => QV a s -> IO (NList a s)
observeV v = do
  let nv = normalize v
      probs = squareModulus . getProb nv <$> basis
  r <- getStdRandom $ randomR (0.0, 1.0)
  let accumulatedProbs = zip (scanl1 (+) probs) basis
      Just (_, res) = find ((r <) . fst) accumulatedProbs
        -- never yields Nothing due to normalization
  return res

observeRef :: Basis a => KnownNat s => QR a s -> IO (NList a s)
observeRef (QR ptr) = do
  qVal <- readIORef ptr
  observResult <- observeV qVal
  writeIORef ptr (mkQV [(observResult, 1)])
  return observResult

_observeN :: forall a predN sMinusN s.
  (KnownNat predN, KnownNat sMinusN, KnownNat s, Basis a)
  => QR a s -> SNat predN -> SNat sMinusN -> IO (NList a 1)
_observeN (QR ptr) SNat SNat = do
  qVal <- readIORef ptr
  let prob' a =
        sqrt . sum
          $ [ squareModulus
              (getProb qVal $ unsafeCoerce (left <+> (a :> NNil) <+> right))
              :+ 0
            | left <- basis @(NList a predN)
            , right <- basis @(NList a sMinusN)
            ]
      auxQval = mkQV [(a :> NNil, prob' a) | a <- basis @a]
  obsRes <- observeV auxQval
  let newVal =
        mkQV ([ ( unsafeCoerce $ left <+> obsRes <+> right
              , getProb qVal $ unsafeCoerce (left <+> obsRes <+> right))
            | left <- basis @(NList a predN)
            , right <- basis @(NList a sMinusN)
            ] :: [(NList a s, PA)])
  writeIORef ptr (normalize newVal)
  return obsRes

observeN ::
     forall a s n. 
     (Basis a, KnownNat s, KnownNat n)
  => QR a s 
  -> SNat n
  -> IO (NList a 1)
observeN qr SNat =
  let sMinusN = someNatVal (natVal (Proxy @s) - natVal (Proxy @n))
      predN   = someNatVal (natVal (Proxy @n) - 1)
  in case (sMinusN, predN) of 
    (Just (SomeNat (Proxy :: Proxy sMinusN')), Just (SomeNat (Proxy :: Proxy predN'))) 
      -> _observeN qr (SNat @predN') (SNat @sMinusN')
    _ -> error "observeN: impossible, this should never happen please report a bug"