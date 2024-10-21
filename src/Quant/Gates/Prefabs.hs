module Gates.Prefabs(
  x
  , y
  , z
  , h
  , p
  , t
  , s
  , cnot
  , entangle
  , sample
  , measure
  , cz
  , fredkin
  , toffoli
  , swap
  , QAct, runQ, app, liftIO)
where

import Gates.QAct 
import Gates.Gates
import Core.Value
import Control.Monad.Reader


x :: KnownNat s => QAct '[ n] s
x = actQop _x

y :: KnownNat s => QAct '[ n] s
y = actQop _y

z :: KnownNat s => QAct '[ n] s
z = actQop _z

p :: KnownNat s => Double -> QAct '[ n] s
p l = actQop (_p l)

t :: KnownNat s => QAct '[ n] s
t = actQop _t

s :: KnownNat s => QAct '[ n] s
s = actQop _s

h :: KnownNat s => QAct '[ n] s
h = actQop _h

cnot :: KnownNat s => QAct '[ n1, n2] s
cnot = actQop _cnot

cz :: KnownNat s => QAct '[ n1, n2] s
cz = actQop _cz

entangle :: KnownNat s => QAct '[ n1, n2] s
entangle = do
  app (#1 :- SNil) h
  app (#1 :- #2 :- SNil) cnot

fredkin :: KnownNat s => QAct '[ n1, n2, n3] s
fredkin = actQop _fredkin

toffoli :: KnownNat s => QAct '[ n1, n2, n3] s
toffoli = actQop _toffoli

swap :: KnownNat s => QAct '[ n1, n2] s
swap = actQop _swap

sample :: QAct acs t
sample = do
  qr <- ask
  liftIO $ printQ qr

measure ::
     forall n acs s. KnownNat s => KnownNat (acs !! n)
  => SNat n
  -> QAct' acs s Bit
measure SNat = do
  qv <- ask
  liftIO $ do
    (k :> NNil) <- measureV qv (SNat @n)
    return k
