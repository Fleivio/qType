module Quant(t1) where 

import Gates.QAct
import List.List
import Core.Value
import Control.Monad.Reader (liftIO)

test :: KnownNat t => KnownNat n1 => QAct '[n1,n2,n3] t
test = do
  sample
  app (#1 :- #2 :- SNil) entangle
  b <- measure #1
  liftIO $ print b
  sample

t1 :: IO ()
t1 = do 
  a <- mkQ [(I:>O:>O:>NNil,1)]
  runQ test a

t2 :: IO ()
t2 = do 
  a <- mkQ [(I:>O:>I:>I:>NNil,1)]
  print =<< measureV a #1
  print =<< measureV a #2
  print =<< measureV a #3
  print =<< measureV a #4
  