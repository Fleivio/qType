module Virt.Gates(
  h, x, y, z, p, s, t, cnot, toffoli, swap, fredkin, module Virt.QState
) where 

import Virt.QState
import Value.Gates

appGate :: forall acs t. 
  ValidDecomposer acs t -- Virt Bin acs t is a valid decomposition of t
  => Basis (NList Bin t)
  => Qop Bin (Length acs) (Length acs) -> QState acs t ()
appGate op = do
  qubit <- get
  liftIO $ app op qubit

appGate' :: forall newAcs acs t. 
  ValidDecomposer newAcs (Length acs) -- newAcs can select over acs
  => ValidDecomposer (Select newAcs acs) t -- newAcs can select over t
  => Basis (NList Bin t)
  => Qop Bin (Length newAcs) (Length newAcs) -> QState acs t ()
appGate' op = do 
  qubit <- get
  _ <- liftIO $ execStateT (appGate @(Select newAcs acs) @t (unsafeCoerce op)) (virtFromV @newAcs qubit)
  return ()

h :: forall n acs t. 
  ValidDecomposer '[n] (Length acs) 
  => ValidDecomposer (Select '[n] acs) t
  => Basis (NList Bin t)
  => Key n -> QState acs t ()
h Key = appGate' @'[n] _h

x :: forall n acs t. 
  ValidDecomposer '[n] (Length acs) 
  => ValidDecomposer (Select '[n] acs) t
  => Basis (NList Bin t)
  => Key n -> QState acs t ()
x Key = appGate' @'[n] _x

y :: forall n acs t. 
  ValidDecomposer '[n] (Length acs) 
  => ValidDecomposer (Select '[n] acs) t
  => Basis (NList Bin t)
  => Key n -> QState acs t ()
y Key = appGate' @'[n] _y


z :: forall n acs t. 
  ValidDecomposer '[n] (Length acs) 
  => ValidDecomposer (Select '[n] acs) t
  => Basis (NList Bin t)
  => Key n -> QState acs t ()
z Key = appGate' @'[n] _z

p :: forall n acs t. 
  ValidDecomposer '[n] (Length acs) 
  => ValidDecomposer (Select '[n] acs) t
  => Basis (NList Bin t)
  => Double -> Key n -> QState acs t ()
p l Key = appGate' @'[n] (_p l)

s :: forall n acs t. 
  ValidDecomposer '[n] (Length acs) 
  => ValidDecomposer (Select '[n] acs) t
  => Basis (NList Bin t)
  => Key n -> QState acs t ()
s Key = appGate' @'[n] _s

t :: forall n acs t. 
  ValidDecomposer '[n] (Length acs) 
  => ValidDecomposer (Select '[n] acs) t
  => Basis (NList Bin t)
  => Key n -> QState acs t ()
t Key = appGate' @'[n] _t

cnot :: forall n1 n2 acs t. 
  ValidDecomposer '[n1,n2] (Length acs) 
  => ValidDecomposer (Select '[n1,n2] acs) t
  => Basis (NList Bin t)
  => Key n1 -> Key n2 -> QState acs t ()
cnot Key Key = appGate' @'[n1,n2] _cnot

swap :: forall n1 n2 acs t. 
  ValidDecomposer '[n1,n2] (Length acs) 
  => ValidDecomposer (Select '[n1,n2] acs) t
  => Basis (NList Bin t)
  => Key n1 -> Key n2 -> QState acs t ()
swap Key Key = appGate' @'[n1,n2] _swap

toffoli :: forall n1 n2 n3 acs t. 
  ValidDecomposer '[n1,n2,n3] (Length acs) 
  => ValidDecomposer (Select '[n1,n2,n3] acs) t
  => Basis (NList Bin t)
  => Key n1 -> Key n2 -> Key n3 -> QState acs t ()
toffoli Key Key Key = appGate' @'[n1,n2,n3] _toffoli

fredkin :: forall n1 n2 n3 acs t. 
  ValidDecomposer '[n1,n2,n3] (Length acs) 
  => ValidDecomposer (Select '[n1,n2,n3] acs) t
  => Basis (NList Bin t)
  => Key n1 -> Key n2 -> Key n3 -> QState acs t ()
fredkin Key Key Key = appGate' @'[n1,n2,n3] _fredkin
