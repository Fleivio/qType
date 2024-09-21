module Gates.QAct(QAct, runQ, app, x, y, z, h, p, t, s, cnot, sample, measure) where

import Control.Monad.Reader
import Gates.Gates
import Core.Value

type QAct' :: [Natural] -> Natural -> Type -> Type
type QAct' acs t a = ReaderT (Virt Bit acs t) IO a

type QAct :: [Natural] -> Natural -> Type
type QAct acs t = QAct' acs t ()

runQ :: QAct (CountTo t) t -> Virt Bit (CountTo t) t -> IO ()
runQ = runReaderT

actQop ::
     Basis (NList Bit s)
  => Qop Bit (Length acs) (Length acs) -> QAct acs s
actQop f' = do
  vv <- ask
  lift $ appV f' vv

app :: ValidSelector newacs acs =>
  SList newacs
  -> QAct (Select newacs acs) t 
  -> QAct acs t
app sl act = do
  qv <- ask
  let adapterQv = selectQ sl qv
  lift $ runReaderT act
                    adapterQv

--------------------------------

x :: 
  Basis (NList Bit s) 
  => QAct '[n] s
x = actQop _x

y :: 
  Basis (NList Bit s) 
  => QAct '[n] s
y = actQop _y

z :: 
  Basis (NList Bit s) 
  => QAct '[n] s
z = actQop _z

p :: 
  Basis (NList Bit s)
  => Double -> QAct '[n] s
p l = actQop (_p l)

t ::
  Basis (NList Bit s) 
  => QAct '[n] s
t = actQop _t

s ::
  Basis (NList Bit s) 
  => QAct '[n] s
s = actQop _s

h ::
  Basis (NList Bit s) 
  => QAct '[n] s
h = actQop _h

cnot ::
  Basis (NList Bit s)  
  => QAct '[n1, n2] s
cnot = actQop _cnot

cz ::
  Basis (NList Bit s) 
  => QAct '[n1, n2] s
cz = actQop _cz

entangle :: 
  Basis (NList Bit s)
  => QAct '[n1, n2] s
entangle = do
  app (SNat @1 :- SNil) h
  app (SNat @1 :- SNat @2 :- SNil) cnot

fredkin ::
  Basis (NList Bit s) 
  => QAct '[n1, n2, n3] s
fredkin = actQop _fredkin

toffoli ::
  Basis (NList Bit s) 
  => QAct '[n1, n2, n3] s
toffoli = actQop _toffoli

swap ::
  Basis (NList Bit s) 
  => QAct '[n1, n2] s
swap = actQop _swap

sample :: QAct acs t
sample = do
  qr <- ask
  liftIO $ printQ qr

measure :: forall n acs s.
  Measureable Bit (Eval (acs !! n)) s
  -- => ValidSelector '[Eval (acs !! n)] s
  => SNat n -> QAct' acs s Bit
measure SNat = do 
  qv <- ask
  liftIO $ do 
    (k:>NNil) <- measureV qv (SNat @n)
    return k