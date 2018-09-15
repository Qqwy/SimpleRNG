{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SimpleRNG where

import Data.Int (Int32)
import Data.Bits (xor, shiftL, shiftR)
import System.Random (RandomGen(..))

newtype SeedState = SeedState Int32
  deriving (Eq, Show, Enum, Bounded)

seed :: Integral a => a -> SeedState
seed = SeedState . fromIntegral

rand_r :: SeedState -> (Int32, SeedState)
rand_r (SeedState a) = (d, SeedState d)
  where
    b = a `xor` (shiftL a 13)
    c = b `xor` (shiftR b 17)
    d = c `xor` (shiftL c 5)

instance RandomGen SeedState where
  next seed_state = (fromIntegral num, new_seed_state)
    where
      (num, new_seed_state) = rand_r seed_state
  genRange seed_state = (fromEnum (minBound `asTypeOf` seed_state),
                fromEnum (maxBound `asTypeOf` seed_state))

  split seed_state@(SeedState num) =  (seed_state', inverted_seed_state')
    where
      (_, seed_state') = next seed_state
      (_, inverted_seed_state') = next inverted_seed_state
      inverted_seed_state = SeedState (maxBound - num)
