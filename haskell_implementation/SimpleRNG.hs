{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SimpleRNG where

import Data.Word (Word32)
import Data.Bits (xor, shift)
import System.Random (RandomGen(..), randoms)
import System.IO
import Control.Monad
import Control.Arrow

(|>) :: a -> (a -> b) -> b
(|>) x f = f x
infixl 0 |>

newtype RNGState = RNGState Word32
  deriving (Eq, Show, Enum, Bounded)

seed :: Integral a => a -> RNGState
seed = RNGState . fromIntegral

rand_r :: RNGState -> (Word32, RNGState)
rand_r (RNGState num) = (res, RNGState res)
  where
    res = num
      |> xorshift 13
      |> xorshift (-17)
      |> xorshift 5
    xorshift :: Int -> Word32 -> Word32
    xorshift amount x = x `xor` (shift x amount)

instance RandomGen RNGState where
  next seed_state = (first fromIntegral) $ rand_r seed_state
    where
  genRange seed_state = (fromEnum (minBound `asTypeOf` seed_state),
                fromEnum (maxBound `asTypeOf` seed_state))

  split seed_state@(RNGState num) =  (seed_state', inverted_seed_state')
    where
      (_, seed_state') = next seed_state
      (_, inverted_seed_state') = next inverted_seed_state
      inverted_seed_state = RNGState (maxBound - num)

main :: IO ()
main = do
  let ourseed = 42 :: Word32
  let rng = (seed ourseed)
  let random_numbers = System.Random.randoms rng :: [Word32]

  Control.Monad.forM_ (take 10 random_numbers) (System.IO.putStrLn . show)
