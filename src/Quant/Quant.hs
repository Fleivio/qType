module Quant( 
  NList(..), Bin(..), Key, mkQ, printQ,
   m1, m2
  ) where 

import Virt.Value
import Gates.Gates
import Gates.Actions


m1 :: IO ()
m1 = do
  qr <- mkQ [(O:>O:>O:>O:>NNil, 1)]
  let qr2 = selectQ @'[1,2] qr
  let qr1 = selectQ @'[1] qr2
  app _h qr1
  app _cnot qr2
  printQ qr

m2 :: IO ()
m2 = do
  qr <- mkQ [(O:>NNil, 1)]
  runQ (h #1) qr 
  printQ qr