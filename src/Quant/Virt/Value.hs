module Virt.Value
  ( Virt(..)
  , module Ref.Observation
  , module Virt.Label
  , virtFromList
  , printVirt
  , virtFromV
  , app
  , measure
  ) where

import           Data.IORef
import           Ref.Observation
import Virt.Label

type Virt :: Type -> [Natural] -> Natural -> Type

data Virt a acs t =
  Virt (QR a t)

virtFromR :: QR a s -> Virt a (CountTo s) s
virtFromR r = Virt r

virtFromList ::
     Basis (NList a s) => [(NList a s, PA)] -> IO (Virt a (CountTo s) s)
virtFromList = fmap virtFromR . qrFromList

printVirt :: Show a => Virt a acs t -> IO ()
printVirt (Virt qr) = do
  printQR qr

virtFromV ::
  forall n a acs t. 
    ValidDecomposer n (Length acs) 
     => Virt a acs t -> Virt a (Select n acs) t
virtFromV = unsafeCoerce

app ::
     forall a acs s. Basis (NList a s)
  => Basis a 
  => ValidDecomposer acs s 
  => Qop a (Length acs) (Length acs) -> Virt a acs s -> IO ()
app f' (Virt (QR ptr)) = do
  qv <- readIORef ptr
  let fqv = normalize $ appQop gf qv
  writeIORef ptr fqv
  where
    gf =
      mkQop
        [ ((ua, ub), getOpProb f' (a, b))
        | ua <- basis @(NList a s)
        , ub <- basis @(NList a s)
        , let (a, na) = decompose @acs ua
              (b, nb) = decompose @acs ub
        , na == nb
        ]

measure ::
    forall a s t n. 
    ValidDecomposer '[s `At` n] t
    => Basis a
    => Basis (NList a t)
    => Basis (NList a (t - s `At` n))
    => Basis (NList a (s `At` n - 1))
    => KnownNat (s `At` n)
    => Virt a s t -> Key n -> IO (NList a 1)
measure (Virt qr) Key = observeN qr (SNat @(s `At` n))