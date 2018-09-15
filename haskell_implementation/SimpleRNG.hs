{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SimpleRNG (
                 seed,
                 rand_r
                 )
where

    {- |
       Implementation of a 32-bit 'Xorshift' Random Number Generator,
       based on the work of George Marsaglia (www.jstatsoft.org/v08/i14/paper , doi:10.18637/jss.v008.i14)

       NOTE: This is _not_ a cryptographically secure random number generator.
       Also, be aware that '0' should not be used as a seed, because in this case, the output of the RNG will always be '0'.

       Author: Wiebe-Marten Wijnja (Qqwy)
       Date: 2018-09-15

       Usage instructions:

       1. use `rng_state = SimpleRNG.seed your_lucky_number`  to seed the RNG to a desired 32-bit unsigned integer.
       2. Call `SimpleRNG.rand rng_state` to get a new random 32-bit unsigned integer.

      Because we're in a pure functional language, you'll keep track of the `rng_state` yourself somewhere.
      For instance, to an infinite stream of random numbers:

      `infinite_stream lucky_number = map fst $ iterate (\(_, rng) -> rand_r rng) (rand_r $ seed lucky_number)`

      NOTE: While this library implements RandomGen, theoretically making it possible to use `take 10 $ System.Random.randoms (SimpleRNG.seed 42)` or the likes,
            this will not result in a deterministic range of values that is the same as the C, Java and Python implementations of this library;
            I do not yet know why; track the progress of this question here:
                  https://stackoverflow.com/questions/52343524/haskell-randomgen-drops-half-of-values


      NOTE2: If you have trouble installing this library, run `cabal install random` first. Since Haskell 7.2.1, `System.Random` is no longer part of core, but extracted to the 'random' library.
    -}

import Data.Word (Word32)
import Data.Bits (xor, shift)
import System.Random (RandomGen(..))
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
