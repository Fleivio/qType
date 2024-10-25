module Gates.Circuit(Circuit(..), mkCircuit) where


data Circuit = Circuit {
    circEntries :: Int,
    circWidth   :: Int
  } deriving (Show)

mkCircuit :: Int -> Int -> Circuit
mkCircuit h w = Circuit h w