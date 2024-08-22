module Quant(m1) where 

import Virt.Value
import Virt.Gates

entangle = do
  h #1
  sample --debug
  cnot #1 #2

m1 :: IO ()
m1 = do
  qr <- virtFromList [(O:>O:>I:>I:>NNil, 1)]
  a <- runQ entangle qr
  printVirt qr


