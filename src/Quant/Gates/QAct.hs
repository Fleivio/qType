module Gates.QAct
  ( QAct, QAct'
  , runQ
  , app
  , actQop
  , getCurrentIndexes
  , runHist
  , tell
  ) where

import           Control.Monad.Reader
import           Control.Monad.Writer
import           Control.Monad
import           Core.Value

import Gates.History

type QAct' :: [Natural] -> Natural -> Type -> Type
type QAct' acs t a = WriterT OpHistory (ReaderT (Virt Bit acs t) IO) a

type QAct :: [Natural] -> Natural -> Type
type QAct acs t = QAct' acs t ()

-- execute a quantum operation and discard the history
runQ :: KnownNat t => QAct (CountTo t) t -> Virt Bit (CountTo t) t -> IO ()
runQ qa val = void $ runHist qa val

-- execute a quantum operation and return the history
runHist :: 
  forall t. KnownNat t => QAct (CountTo t) t -> Virt Bit (CountTo t) t -> IO OpHistory
runHist = runHist'

runHist' :: 
  forall t acs. QAct acs t -> Virt Bit acs t -> IO OpHistory
runHist' qa val = snd <$> runReaderT (runWriterT qa) val

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
  hist <- liftIO $ runHist' act adapterQv
  tell hist

getCurrentIndexes :: QAct' acs t [Int]
getCurrentIndexes = do
  Virt _ acs <- ask
  return $ unsafeCoerce acs