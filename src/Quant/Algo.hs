module Algo(adder, deutsch, teleport) where

import Gates.QAct
import Core.Value


adder :: KnownNat t 
  => QAct '[n1, n2, carryIn, carryOut] t
adder = do
  app (#1 :- #2 :- #4 :- SNil) toffoli
  app (#1 :- #2 :- SNil) cnot
  app (#2 :- #3 :- #4 :- SNil) toffoli
  app (#2 :- #3 :- SNil) cnot
  app (#1 :- #2 :- SNil) cnot

deutsch :: KnownNat t => KnownNat n1 => QAct '[n1, n2] t -> QAct '[n1, n2] t
deutsch uf = do
  app (#1 :- SNil) h
  app (#2 :- SNil) h
  app (#1 :- #2 :- SNil) uf
  app (#1 :- SNil) h
  val <- measure #1
  case val of
    O -> liftIO $ print "f is constant"
    I -> liftIO $ print "f is balanced"

teleport :: KnownNat t => KnownNat n1 => KnownNat n2
  => QAct '[n1, n2, n3] t
teleport = do
  app (#2 :- #3 :- SNil) entangle
  app (#1 :- #2 :- SNil) cnot
  app (#1 :- SNil) h
  measure #1
  measure #2
  app (#2 :- #3 :- SNil) cnot
  app (#1 :- #3 :- SNil) cz
