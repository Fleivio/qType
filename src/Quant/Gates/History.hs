module Gates.History(OpRep(..), OpHistory) where

data OpRep = 
    ClusterOp [OpRep]
  | SwapCross Int
  | StringOp String Int
  | CircledPlus Int
  | FullCircle Int
  | Measure Int
  | Sample String

instance Show OpRep where
  show (ClusterOp ops) = "ClusterOp " ++ show ops
  show (SwapCross i) = "SwapCross " ++ show i
  show (StringOp s i) = "StringOp " ++ s ++ " " ++ show i
  show (CircledPlus i) = "CircledPlus " ++ show i
  show (FullCircle i) = "FullCircle " ++ show i
  show (Measure i) = "Measure " ++ show i
  show (Sample s) = "Sample " ++ s

type OpHistory = [OpRep]