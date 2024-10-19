module Quant(t1) where 

import Gates.QAct
import List.List
import Core.Value

test :: KnownNat t => QAct '[n1,n2,n3] t
test = do
  sample
  app (#1 :- #3 :- SNil) entangle
  sample

t1 :: IO ()
t1 = do 
  a <- mkQ [(I:>O:>O:>NNil,1)]
  runQ test a