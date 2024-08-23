module Quant( 
  NList(..), Bin(..), Key, mkQ, printQ,
   m1
  ) where 

import Virt.Value
import Gates.Gates

m1 :: IO ()
m1 = do 
  qr <- mkQ [(O:>O:>O:>O:>NNil, 1)]
  let qr1 = selectQ @'[1] qr
  let qr2 = selectQ @'[1,3] qr
  app _h qr1
  app _cnot qr2
  printQ qr

