module Quant( 
  NList(..), Bit(..), Key, mkQ, printQ,
   m1
  ) where 

import Core.Value
import Gates.Gates
import List.SList
import Gates.QAct


m1 :: IO ()
m1 = do
  qr <- mkQ [(O:>O:>O:>O:>NNil, 1)]
  let qr2 = selectQ (#1 :>> #2 :>> SNil) qr
  let qr1 = selectQ (#1 :>> SNil) qr2
  appV _h qr1
  appV _cnot qr2
  printQ qr

-- test :: forall n1 n2 n3 t.   
--   Basis (NList Bit t) 
--   => ValidDecomposer '[n1, n2, n3] t
--   => ValidDecomposer '[n1, n2] t
--   => ValidDecomposer '[n1] t
--   => QAct '[n1, n2, n3] t
-- test = do
--   app (#1 :>> SNil) (h #1)
--   sample

-- m2 :: IO ()
-- m2 = do
--   qr <- mkQ [(O:>O:>O:>O:>NNil, 1)]
--   runQ (app (#1 :>> #2 :>> #4 :>> SNil) (test) qr


  