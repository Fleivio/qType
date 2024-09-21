module Quant(t1) where 

import Gates.QAct
import List.List
import Core.Value

t1 = do 
  a <- mkQ [(O:>I:>O:>NNil,1)]
  runQ (app (SNat @1 :- SNat @4 :- SNil) cnot) a
  printQ a