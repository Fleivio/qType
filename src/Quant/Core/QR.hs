module Core.QR
  ( mkQR
  , QR(..)
  , qrApp
  , printQR
  , showQR
  , module Core.Operator
  , qrFromList
  ) where

import Data.IORef
import Core.Operator

newtype QR a s =
  QR (IORef (QV a s))

mkQR :: QV a s -> IO (QR a s)
mkQR qv = QR <$> newIORef qv

qrFromList :: (Basis a, KnownNat s) => [(NList a s, PA)] -> IO (QR a s)
qrFromList lst = mkQR $ mkQV lst

qrApp :: (Basis a, KnownNat s) => Ord a => Qop a s s -> QR a s -> IO ()
qrApp op (QR ref) = modifyIORef ref (appQop op)

showQR :: Show a => QR a s -> IO String
showQR (QR ref) = do
  qval <- readIORef ref
  return $ showQV qval

printQR :: (Show a) => QR a s -> IO ()
printQR (QR ref) = do
  qval <- readIORef ref
  putStrLn $ showQV qval