{-#LANGUAGE OverloadedLabels#-}

module Main where

import Quant

main :: IO ()
main = do
  m1
  m2

-- top level binding would be quite annoying
-- TODO: better way to handle this]
-- TODO: better errors (no repetition, etc)
