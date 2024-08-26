module Quant( 
  NList(..), Bit(..), Key, mkQ, printQ,
   m1
  ) where 

import Core.Value
import Gates.Gates
import List.Key


m1 :: IO ()
m1 = do
  qr <- mkQ [(O:>O:>O:>O:>NNil, 1)]
  let qr2 = selectQ @'[1,2] qr
  let qr1 = selectQ @'[1] qr2
  app _h qr1
  app _cnot qr2
  printQ qr

