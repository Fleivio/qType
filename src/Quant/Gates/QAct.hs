module Gates.QAct(QAct, h, cnot, x, y, z, p, t, s, toffoli, fredkin, swap, sample, measure, app, runQ) where

import Control.Monad.Reader
import Gates.Gates
import Core.Value
import List.SList

type QAct :: [Natural] -> Natural -> Type
type QAct acs t = ReaderT (Virt Bit acs t) IO ()

runQ :: ValidDecomposer (CountTo t) t
  => Basis (NList Bit t)
  => QAct (CountTo t) t -> Virt Bit (CountTo t) t  -> IO ()
runQ = runReaderT 

actQop ::
     forall acs s. Basis (NList Bit s)
  => ValidDecomposer acs s
  => Qop Bit (Length acs) (Length acs) -> QAct acs s
actQop f' = do
  vv <- ask
  lift $ appV f' vv

app :: forall nacs acs t. 
  ValidDecomposer nacs (Length acs)
  => QAct (Select nacs acs) t
  -> SList nacs
  -> QAct acs t
app act sl = do
  qv <- ask
  let adapterQv = selectQ sl qv
  lift $ runReaderT act
                    adapterQv

-- class AppAux returnValue where
--   app' :: 
    

{-
entangle :: forall n1 n2 s.
  Basis (NList Bit s) 
  => ValidDecomposer '[n1, n2] s
  => ValidDecomposer '[n1] s
  => Key n1 -> Key n2 -> QAct '[n1, n2] s
entangle = do
  app' h #1
  app' cnot #1 #2

  -- app' (p 3) #1

-}

--------------------------------

x :: forall n s.
  Basis (NList Bit s) 
  => ValidDecomposer '[n] s 
  => Key n -> QAct '[n] s
x Key = actQop @'[n] _x

y :: forall n s.
  Basis (NList Bit s) 
  => ValidDecomposer '[n] s 
  => Key n -> QAct '[n] s
y Key = actQop @'[n] _y

z :: forall n s.
  Basis (NList Bit s) 
  => ValidDecomposer '[n] s 
  => Key n -> QAct '[n] s
z Key= actQop @'[n] _z

p :: forall n s.
  Basis (NList Bit s) 
  => ValidDecomposer '[n] s 
  => Double -> Key n -> QAct '[n] s
p l Key = actQop @'[n] (_p l)

t :: forall n s.
  Basis (NList Bit s) 
  => ValidDecomposer '[n] s 
  => Key n -> QAct '[n] s
t Key = actQop @'[n] _t

s :: forall n s.
  Basis (NList Bit s) 
  => ValidDecomposer '[n] s 
  => Key n -> QAct '[n] s
s Key = actQop @'[n] _s

h :: forall n s.
  Basis (NList Bit s) 
  => ValidDecomposer '[n] s 
  => Key n -> QAct '[n] s
h Key = actQop @'[n] _h

cnot :: forall n1 n2 s.
  Basis (NList Bit s) 
  => ValidDecomposer '[n1, n2] s 
  => Key n1 -> Key n2 -> QAct '[n1, n2] s
cnot Key Key = actQop @'[n1, n2] _cnot

-- entangle :: forall n1 n2 s.
--   Basis (NList Bit s) 
--   => ValidDecomposer '[n1, n2] s
--   => ValidDecomposer '[n1] s -- fix that
--   => QAct '[n1, n2] s
-- entangle = do
--   app @'[1] h
--   app @'[1,2] cnot

fredkin :: forall n1 n2 n3 s.
  Basis (NList Bit s) 
  => ValidDecomposer '[n1, n2, n3] s 
  => Key n1 -> Key n2 -> Key n3 -> QAct '[n1, n2, n3] s
fredkin Key Key Key = actQop @'[n1, n2, n3] _fredkin

toffoli :: forall n1 n2 n3 s.
  Basis (NList Bit s) 
  => ValidDecomposer '[n1, n2, n3] s 
  => Key n1 -> Key n2 -> Key n3 -> QAct '[n1, n2, n3] s
toffoli Key Key Key = actQop @'[n1, n2, n3] _toffoli

swap :: forall n1 n2 s.
  Basis (NList Bit s) 
  => ValidDecomposer '[n1, n2] s 
  => Key n1 -> Key n2 -> QAct '[n1, n2] s
swap Key Key = actQop @'[n1, n2] _swap

sample :: QAct acs t
sample = do
  qr <- ask
  liftIO $ printQ qr

measure :: forall n acs s.
  Basis (NList Bit s) 
  => Basis (NList Bit (s - At acs n))
  => Basis (NList Bit (At acs n - 1))
  => KnownNat (At acs n)
  => ValidDecomposer '[acs `At` n] s
  => QAct acs s
measure = do 
  qv <- ask
  liftIO $ measureV qv (Key @n) >> return ()