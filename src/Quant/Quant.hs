module Quant(t1) where 

import Gates.Prefabs
import List.List
import Core.Value
import List.Quoter

import Algo

test :: KnownNat t => KnownNat n1 => QAct '[n1,n2,n3] t
test = do
  sample
  app (#1 :- #2 :- SNil) entangle
  sample
  b <- measure #1
  liftIO $ print b
  sample

test2 :: KnownNat t => QAct '[n1,n2,n3,n4,n5,n6] t
test2 = do
  app (#1 :- #2 :- #3 :- #4 :- SNil) adder
  sample

t1 :: IO ()
t1 = do 
  -- a <- mkQ [(I:>I:>I:>O:>I:>I:>NNil,1)]
  -- runQ test2 a
  -- printQ a

  -- print "-----"

  -- b <- mkQ [(O:>O:>O:>NNil,1)]
  -- runQ test b

  -- print "-----"

  c <- mkQ [(O:>O:>O:>NNil,1), (I:>O:>O:>NNil,1)]
  cir <- runHist (teleport) c
  print cir