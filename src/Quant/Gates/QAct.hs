module Gates.QAct
  ( QAct
  , runQ
  , app
  , x
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
  ) where

import           Control.Monad.Reader
import           Core.Value
import           Gates.Gates

type QAct' :: [Natural] -> Natural -> Type -> Type
type QAct' acs t a = ReaderT (Virt Bit acs t) IO a

type QAct :: [Natural] -> Natural -> Type
type QAct acs t = QAct' acs t ()

runQ :: QAct (CountTo t) t -> Virt Bit (CountTo t) t -> IO ()
runQ = runReaderT

actQop :: KnownNat s => Qop Bit (Length acs) (Length acs) -> QAct acs s
actQop f' = do
  vv <- ask
  lift $ appV f' vv

app ::
     ValidSelector newacs acs
  => SList newacs
  -> QAct (Select newacs acs) t
  -> QAct acs t
app sl act = do
  qv <- ask
  let adapterQv = selectQ sl qv
  lift $ runReaderT act adapterQv

--------------------------------
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
     forall n acs s. KnownNat s => KnownNat (acs !!! n)
  => SNat n
  -> QAct' acs s Bit
measure SNat = do
  qv <- ask
  liftIO $ do
    (k :> NNil) <- measureV qv (SNat @n)
    return k
