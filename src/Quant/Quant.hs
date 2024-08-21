module Quant(m1) where 

import Value.Operator
import Ref.Observation

m1 = do
  qr <- qrFromList [(True:>True:>True:>NNil, 1), (True:>True:>False:>NNil, 1)]
  observeN qr (SNat @3)
  printQR qr