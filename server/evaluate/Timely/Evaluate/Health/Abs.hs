module Timely.Evaluate.Health.Abs where


newtype Abs a = Abs
  { value :: a }
  deriving (Show, Eq)


absolute :: Num a => a -> Abs a
absolute a = Abs (abs a)


