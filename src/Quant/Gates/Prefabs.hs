module Gates.Prefabs(
  x
  , y
  , z
  , h
  , p
  , t
  , s
  , cnot
  , entangle
  , sample
  , measure
  , cz
  , fredkin
  , toffoli
  , swap
  , QAct, runQ, runHist, app, liftIO)
where

import Gates.QAct 
import Gates.Gates
import Core.Value
import Control.Monad.Reader
import Data.Proxy
import Gates.History


x :: KnownNat s => QAct '[ n] s
x = do 
  actQop _x 
  [ind] <- getCurrentIndexes
  tell [StringOp "X" ind]

y :: KnownNat s => QAct '[ n] s
y = do 
  actQop _y
  [ind] <- getCurrentIndexes
  tell [StringOp "Y" ind]

z :: KnownNat s => QAct '[ n] s
z = do 
  actQop _z
  [ind] <- getCurrentIndexes
  tell [StringOp "Z" ind]

p :: KnownNat s => Double -> QAct '[ n] s
p l = do 
  actQop (_p l)
  [ind] <- getCurrentIndexes
  tell [StringOp ("P("<>show l<>")") ind]

t :: KnownNat s => QAct '[ n] s
t = do 
  actQop _t
  [ind] <- getCurrentIndexes
  tell [StringOp "T" ind]

s :: KnownNat s => QAct '[ n] s
s = do
  actQop _s
  [ind] <- getCurrentIndexes
  tell [StringOp "S" ind]

h :: KnownNat s => QAct '[ n] s
h = do 
  actQop _h
  [ind] <- getCurrentIndexes
  tell [StringOp "H" ind]

cnot :: KnownNat s => QAct '[ n1, n2] s
cnot = do 
  actQop _cnot
  [cnotControl, cnotTarget] <- getCurrentIndexes
  tell [ClusterOp [FullCircle cnotControl, CircledPlus cnotTarget]]

cz :: KnownNat s => QAct '[ n1, n2] s
cz = do 
  actQop _cz
  [czControl, czTarget] <- getCurrentIndexes
  tell [ClusterOp [FullCircle czControl, StringOp "Z" czTarget]]

entangle :: KnownNat s => QAct '[ n1, n2] s
entangle = do
  app (#1 :- SNil) h
  app (#1 :- #2 :- SNil) cnot

toffoli :: KnownNat s => QAct '[ n1, n2, n3] s
toffoli = do 
  actQop _toffoli
  [tfControl, tfControl2, tfTarget] <- getCurrentIndexes
  tell [ClusterOp [FullCircle tfControl, FullCircle tfControl2, CircledPlus tfTarget]]

swap :: KnownNat s => QAct '[ n1, n2] s
swap = do 
  actQop _swap
  [swap1, swap2] <- getCurrentIndexes
  tell [ClusterOp [SwapCross swap1, SwapCross swap2]]

fredkin :: KnownNat s => QAct '[ n1, n2, n3] s
fredkin = do 
  actQop _fredkin
  [fredControl, fredTarget1, fredTarget2] <- getCurrentIndexes
  tell [ClusterOp [FullCircle fredControl, SwapCross fredTarget1, SwapCross fredTarget2]]

sample :: QAct acs t
sample = do
  qr <- ask
  str <- liftIO $ showQ qr
  liftIO $ putStrLn str
  tell [Sample str]

measure ::
     forall n acs s. KnownNat s => KnownNat (acs !! n)
  => SNat n
  -> QAct' acs s Bit
measure SNat = do
  qv <- ask
  tell [Measure (fromIntegral $ natVal (Proxy :: Proxy (acs !! n)))]
  liftIO $ do
    (k :> NNil) <- measureV qv (SNat @n)
    return k
