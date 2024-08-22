{-#LANGUAGE OverloadedLabels#-}

module Main where

import Quant

main :: IO ()
main = test

-- top level binding would be quite annoying
-- TODO: better way to handle this]
-- TODO: better errors (no repetition, etc)
_entangle = do
  h #1
  sample --debug
  cnot #1 #2

test :: IO ()
test = do
  qr <- mkQ [(O:>O:>I:>I:>NNil, 1)]
  a <- runQ _entangle qr
  printQ qr


