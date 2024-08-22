module Virt.QState(
  runQ, sample, QState, module Virt.Value, module Control.Monad.State
) where

import Control.Monad.State
import Virt.Value

type QState acs t a = StateT (Virt Bin acs t) IO a

runQ :: QState acs t a -> Virt Bin acs t -> IO (Virt Bin acs t)
runQ = execStateT

sample :: QState acs t ()
sample = do
  qubit <- get
  liftIO $ printVirt qubit
  return ()