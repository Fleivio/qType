module Gates.QAct
  ( QAct, QAct'
  , runQ
  , app
  , actQop
  ) where

import           Control.Monad.Reader
import           Core.Value

type QAct' :: [Natural] -> Natural -> Type -> Type
type QAct' acs t a = ReaderT (Virt Bit acs t) IO a

type QAct :: [Natural] -> Natural -> Type
type QAct acs t = QAct' acs t ()

runQ :: QAct (CountTo t) t -> Virt Bit (CountTo t) t -> IO ()
runQ = runReaderT

actQop :: KnownNat s => Qop Bit (Length acs) (Length acs) -> QAct acs s
actQop f' = do
  vv <- ask
  lift $ appV f' vv

app ::
     ValidSelector newacs acs
  => SList newacs
  -> QAct (Select newacs acs) t
  -> QAct acs t
app sl act = do
  qv <- ask
  let adapterQv = selectQ sl qv
  lift $ runReaderT act adapterQv
