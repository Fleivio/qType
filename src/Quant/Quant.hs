module Quant(t1) where 

import Gates.QAct
import List.List
import Core.Value

test :: Basis (NList Bit t) => QAct '[n1,n2,n3] t
test = do
  sample
  app (#1 :- SNil) h
  sample
  app (#1 :- SNil) h
  sample

t1 = do 
  a <- mkQ [(O:>I:>O:>NNil,1)]
  runQ (test) a