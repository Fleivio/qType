module Virt.QAction(runQ, QAction, module Virt.Value, module Control.Monad.State) where

import Control.Monad.State
import Virt.Value

type QAction' a acs t = StateT (Virt a acs t) IO ()
type QAction acs t = QAction' Bin acs t

runQ :: forall acs t. QAction acs t -> Virt Bin acs t -> IO ()
runQ act qv = do
  _ <- execStateT act qv
  return ()
