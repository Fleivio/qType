
module Core.Value
  ( Virt(..)
  , module Core.Observation
  , mkQ
  , printQ
  , selectQ
  , appV
  , measureV
  ) where

import           Data.IORef
import           Core.Observation

type Virt :: Type -> [Natural] -> Natural -> Type

data Virt a acs t where
  Virt :: QR a t -> SList acs -> Virt a acs t

virtFromR :: KnownNat s => QR a s -> Virt a (CountTo s) s
virtFromR (qr :: QR a s) = Virt qr (sListCountTo (SNat @s))

mkQ :: KnownNat s => Basis (NList a s) => [(NList a s, PA)] -> IO (Virt a (CountTo s) s)
mkQ = fmap virtFromR . qrFromList

printQ :: Show a => Virt a acs t -> IO ()
printQ (Virt qr _) = do
  printQR qr

selectQ ::
  forall nacs a acs t. SList nacs -> Virt a acs t -> Virt a (Select nacs acs) t
selectQ sl (Virt qr acs) = Virt qr (selectSL sl acs)

appV ::
     forall a acs s. Basis (NList a s)
  => Basis a 
  => Qop a (Length acs) (Length acs) -> Virt a acs s -> IO ()
appV f' (Virt (QR ptr) acs) = do
  qv <- readIORef ptr
  let fqv = normalize $ appQop gf qv
  writeIORef ptr fqv
  where
    gf =
      mkQop
        [ ((ua, ub), getOpProb f' (a, b))
        | ua <- basis @(NList a s)
        , ub <- basis @(NList a s)
        , let (a, na) = decompose acs ua
              (b, nb) = decompose acs ub
        , na == nb
        ]

measureV ::
    forall a s t n. 
     Measureable a (Eval (s !! n)) t
    => Virt a s t -> SNat n -> IO (NList a 1)
measureV (Virt qr _) SNat = observeN qr (SNat @(Eval (s !! n)))