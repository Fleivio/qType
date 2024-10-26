module Gates.QAct
  ( QAct, QAct'
  , runQ
  , app
  , actQop
  , getCurrentIndexes
  , runCirc
  ) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad
import           Core.Value

import Data.Proxy
import Gates.Circuit

type QAct' :: [Natural] -> Natural -> Type -> Type
type QAct' acs t a = StateT Circuit (ReaderT (Virt Bit acs t) IO) a

type QAct :: [Natural] -> Natural -> Type
type QAct acs t = QAct' acs t ()

runQ :: KnownNat t => QAct (CountTo t) t -> Virt Bit (CountTo t) t -> IO ()
runQ qa val = void $ runCirc qa val

runCirc :: forall t. KnownNat t => QAct (CountTo t) t -> Virt Bit (CountTo t) t -> IO Circuit
runCirc qa val = snd <$> runReaderT (runStateT qa baseCircuit) val
  where 
    baseCircuit = mkCircuit entryCount 0
    entryCount = fromIntegral $ natVal (Proxy @t)

runQ' :: QAct' acs t a -> Virt Bit acs t -> Circuit -> IO (a, Circuit)
runQ' qa val circ = runReaderT (runStateT qa circ) val

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
  circ <- get
  (_, c2) <- liftIO $ runQ' act adapterQv circ 
  put c2



getCurrentIndexes :: QAct' acs t [Int]
getCurrentIndexes = do
  Virt _ acs <- ask
  return $ unsafeCoerce acs