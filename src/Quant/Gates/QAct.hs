module Gates.QAct
  ( QAct, QAct'
  , runQ
  , app
  , actQop
  , orc
  ) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad
import           Core.Value

type QAct' :: [Natural] -> Natural -> Type -> Type
type QAct' acs t a = StateT Int (ReaderT (Virt Bit acs t) IO) a


type QAct :: [Natural] -> Natural -> Type
type QAct acs t = QAct' acs t ()

runQ :: QAct (CountTo t) t -> Virt Bit (CountTo t) t -> IO ()
runQ qa = void . runReaderT (runStateT qa 0)

orc :: QAct (CountTo t) t -> Virt Bit (CountTo t) t -> IO Int
orc qa val = snd <$> runReaderT (runStateT qa 0) val

actQop :: KnownNat s => Qop Bit (Length acs) (Length acs) -> QAct acs s
actQop f' = do
  vv <- ask
  liftIO $ appV f' vv

app ::
     ValidSelector newacs acs
  => SList newacs
  -> QAct (Select newacs acs) t
  -> QAct acs t
app sl act = do
  qv <- ask
  let adapterQv = selectQ sl qv
  liftIO $ runQ' act adapterQv
    where 
      runQ' qa = void . runReaderT (runStateT qa 0)
