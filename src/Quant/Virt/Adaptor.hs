module Virt.Adaptor
  ( Adaptor(..)
  ) where

import Ref.Observation

data Adaptor a s t = Adaptor
  { dec :: NList a t -> NList a s
  , cmp :: NList a s -> NList a t
  }
