module Quant( 
  NList(..), Bin(..), Key, runQ, mkQ, printQ,
  h, x, y, z, toffoli, cnot, p, s, t, fredkin, swap,
  sample, measure
  ) where 

import Virt.Value
import Virt.Gates

mkQ ::
     Basis (NList a s) => [(NList a s, PA)] -> IO (Virt a (CountTo s) s)
mkQ = virtFromList

printQ :: Show a => Virt a acs t -> IO ()
printQ = printVirt