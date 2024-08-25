module Gates.Actions(module Virt.QAction, h, cnot) where

import Virt.QAction
import Gates.Gates

appGate :: forall acs t.
  ValidDecomposer acs t  
  => Basis (NList Bin t)
  => Qop Bin (Length acs) (Length acs) 
  -> QAction acs t
appGate op = do
  qr <- get
  liftIO $ app op qr

appGate' :: forall nacs acs t.
   ValidSubset nacs acs t
  => Basis (NList Bin t)
  => Qop Bin (Length nacs) (Length nacs) 
  -> QAction acs t
appGate' op = do
  modify (unsafeCoerce . selectQ @nacs)
  _ <- unsafeCoerce (appGate op :: QAction nacs t)
  return ()

h :: forall n t acs. 
  ValidSubset '[n] acs t
  => Basis (NList Bin t)
  => Key n -> QAction acs t
h Key = appGate' @'[n] _h

cnot :: forall n1 n2 t acs.
  ValidSubset '[n1, n2] acs t
  => Basis (NList Bin t)
  => Key n1 -> Key n2 -> QAction acs t
cnot Key Key = appGate' @'[n1, n2] _cnot

-- entangle :: forall n1 n2 t acs.
--   ValidSubset '[n1, n2] acs t
--   => Basis (NList Bin t)
--   => KnownNat n1
--   => Key n1 -> Key n2 -> QAction acs t
-- entangle Key Key = do
--   h #1
--   cnot #1 #2