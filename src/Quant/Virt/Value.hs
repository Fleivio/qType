module Virt.Value
  ( Virt(..)
  , module Ref.Observation
  , virtFromR
  , virtFromList
  , printVirt
  , virtFromV
  ) where

import Data.IORef
import Ref.Observation

type Virt :: Type -> [Natural] -> Natural -> Type
data Virt a acs t =
  Virt (QR a t)

virtFromR :: QR a s -> Virt a (CountTo s) s
virtFromR r = Virt r 

virtFromList :: Basis (NList a s) => [(NList a s, PA)] -> IO (Virt a (CountTo s) s)
virtFromList = fmap virtFromR . qrFromList

printVirt :: Show a => Virt a acs t -> IO ()
printVirt (Virt qr) = do 
  printQR qr

virtFromV :: Virt a acs t -> SList n -> Virt a (Select n acs) t
virtFromV v _ = unsafeCoerce v 

app :: forall a acs1 acs2 s t. Basis (NList a s) => Basis (NList a t) => Basis a =>
  Qop a (Lenght acs1) (Lenght acs2) -> Virt a acs1 s -> Virt a acs2 t -> IO ()
app f'@(Qop f) (Virt (QR ra)) (Virt (QR rb)) = do
  fa <- readIORef ra
  let fb = normalize $ appQop gf fa
  writeIORef rb fb
  where
    gf =
      mkQop
        [ ((ua, ub), getOpProb f' (a, b))
        | ua <- basis @(NList a s)
        , ub <- basis @(NList a t)
        , let (a, na) = decompose (SList @acs1) ua
              (b, nb) = decompose (SList @acs2) ub
        , na == nb
        ]

-- app ::
--      (Basis a, Basis b, Basis nab, Basis ua, Basis ub)
--   => Qop a b
--   -> Virt a nab ua
--   -> Virt b nab ub
--   -> IO ()
-- app (Qop f) (Virt (QR ra) (Adaptor {dec = deca, cmp = _})) (Virt (QR rb) (Adaptor { dec = decb
--                                                                                   , cmp = _
--                                                                                   })) = do
--   fa <- readIORef ra
--   let fb = normalize $ appQop gf fa
--   writeIORef rb fb
--   where
--     gf =
--       mkQop
--         [ ((ua, ub), getProb f (a, b))
--         | (ua, ub) <- basis
--         , let (a, na) = deca ua
--               (b, nb) = decb ub
--         , na == nb
--         ]

-- app1 :: (Basis a, Basis na, Basis ua) => Qop a a -> Virt a na ua -> IO ()
-- app1 f v = app f v v

-- observeVV :: (Basis a, Basis na, Basis ua) => Virt a na ua -> IO a
-- observeVV (Virt (QR r) (Adaptor {dec = dec1, cmp = cmp1})) = do
--   qVal <- readIORef r
--   let virtualProb a =
--         sqrt . sum
--           $ [squareModulus (getProb qVal (cmp1 (a, na))) :+ 0 | na <- basis]
--       virtualQVal = mkQV [(a, virtualProb a) | a <- basis]
--   observResult <- observeV virtualQVal
--   let nv =
--         mkQV
--           [ (u, getProb qVal (cmp1 (observResult, na)))
--           | u <- basis
--           , let (a, na) = dec1 u
--           , a == observResult
--           ]
--   writeIORef r $ normalize nv
--   return observResult