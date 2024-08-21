module Virt.Value
  ( Virt(..)
  , virtFromR
  , virtFromV
  , app
  , app1
  , observeVV
  , module Reference.Observation
  , module Virt.Adaptor
  , virtFromList
  , printVirt
  ) where

import Data.IORef
import Ref.Observation

import Virt.Adaptor (Adaptor(..))

data Virt a s t =
  Virt (QR a s) (Adaptor a t s)

type Virt' a s = Virt a s s

virtFromR :: QR a s -> Virt' a s
virtFromR r = Virt r (Adaptor {dec = \a -> (a, ()), cmp = \(a, ()) -> a})

virtFromList :: Basis a => [(a, PA)] -> IO (Virt a () a)
virtFromList = fmap virtFromR . qrFromList

printVirt :: (Basis a, Basis na, Basis ua) => (Show ua) => Virt a na ua -> IO ()
printVirt (Virt qr _) = do 
  printQR qr

virtFromV :: Virt a na u -> Adaptor (a1, a2) a -> Virt a1 (a2, na) u
virtFromV (Virt r gAdaptor) lAdaptor = Virt r composedAdaptor
  where
    gdec = dec gAdaptor
    gcmp = cmp gAdaptor
    ldec = dec lAdaptor
    lcmp = cmp lAdaptor
    composedAdaptor = Adaptor {dec = composedDec, cmp = composedCmp}
    composedCmp (a1, (a2, na)) = gcmp (lcmp (a1, a2), na)
    composedDec u =
      let (a, na) = gdec u
          (a1, a2) = ldec a
       in (a1, (a2, na))

app ::
     (Basis a, Basis b, Basis nab, Basis ua, Basis ub)
  => Qop a b
  -> Virt a nab ua
  -> Virt b nab ub
  -> IO ()
app (Qop f) (Virt (QR ra) (Adaptor {dec = deca, cmp = _})) (Virt (QR rb) (Adaptor { dec = decb
                                                                                  , cmp = _
                                                                                  })) = do
  fa <- readIORef ra
  let fb = normalize $ appQop gf fa
  writeIORef rb fb
  where
    gf =
      mkQop
        [ ((ua, ub), getProb f (a, b))
        | (ua, ub) <- basis
        , let (a, na) = deca ua
              (b, nb) = decb ub
        , na == nb
        ]

app1 :: (Basis a, Basis na, Basis ua) => Qop a a -> Virt a na ua -> IO ()
app1 f v = app f v v

observeVV :: (Basis a, Basis na, Basis ua) => Virt a na ua -> IO a
observeVV (Virt (QR r) (Adaptor {dec = dec1, cmp = cmp1})) = do
  qVal <- readIORef r
  let virtualProb a =
        sqrt . sum
          $ [squareModulus (getProb qVal (cmp1 (a, na))) :+ 0 | na <- basis]
      virtualQVal = mkQV [(a, virtualProb a) | a <- basis]
  observResult <- observeV virtualQVal
  let nv =
        mkQV
          [ (u, getProb qVal (cmp1 (observResult, na)))
          | u <- basis
          , let (a, na) = dec1 u
          , a == observResult
          ]
  writeIORef r $ normalize nv
  return observResult