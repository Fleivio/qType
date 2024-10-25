module Algo(adder, deutsch, teleport) where

import Gates.Prefabs
import Core.Value
import List.Quoter


adder :: KnownNat t 
  => QAct '[n1, n2, carryIn, carryOut] t
adder = do
  app [qb|1 2 4|] toffoli
  app [qb|1 2|]   cnot
  app [qb|2 3 4|] toffoli
  app [qb|2 3|]   cnot
  app [qb|1 2|]   cnot

deutsch :: (KnownNat t, KnownNat n1) => QAct '[n1, n2] t -> QAct '[n1, n2] t
deutsch uf = do
  app [qb|1|]   h
  app [qb|2|]   h
  app [qb|1 2|] uf
  app [qb|1|]   h
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
