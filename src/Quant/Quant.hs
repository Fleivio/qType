module Quant( 
  NList(..), Bit(..), Key, mkQ, printQ,
   m1
  ) where 

import Core.Value
import Gates.Gates
import List.Key
import Gates.QAct


m1 :: IO ()
m1 = do
  qr <- mkQ [(O:>O:>O:>O:>NNil, 1)]
  let qr2 = selectQ @'[1,2] qr
  let qr1 = selectQ @'[1] qr2
  appV _h qr1
  appV _cnot qr2
  printQ qr

test :: forall n1 n2 n3 t.   
  Basis (NList Bit t) 
  => ValidDecomposer '[n1, n2, n3] t
  => ValidDecomposer '[n1, n2] t
  => ValidDecomposer '[n1] t
  => QAct '[n1, n2, n3] t
test = do
  app @'[2] h
  sample
  app @'[1,2] cnot
  sample
  app @'[1,2,3] fredkin
  sample

m2 :: IO ()
m2 = do
  qr <- mkQ [(O:>O:>O:>O:>NNil, 1)]
  runQ (app @'[3,4,1] test) qr


  