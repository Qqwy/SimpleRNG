{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SimpleRNG (
                 seed,
                 random,
                 randoms
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
       2. Call `(random_number, new_rng_state) =  SimpleRNG.rand rng_state` to get a new random 32-bit unsigned integer.
       3. Repeat step (2) as often as you'd like.

      Because we're in a pure functional language, you'll keep track of the `rng_state` yourself somewhere.
      It's very common to want an infinite stream of random numbers.
      For this common pattern, the wrapper function `randoms` has been provided.


      NOTE: While this library implements RandomGen, theoretically making it possible to use it instead of other built-in Haskell random generators, because of the conversion between potential randomness-result types, it is not possible to use it for gaining the same deterministic sequence of values of the implementations of this library in other languages; More information: https://stackoverflow.com/questions/52343524/haskell-randomgen-drops-half-of-values


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

random :: RNGState -> (Word32, RNGState)
random (RNGState num) = (res, RNGState res)
  where
    res = num
      |> xorshift 13
      |> xorshift (-17)
      |> xorshift 5
    xorshift :: Int -> Word32 -> Word32
    xorshift amount x = x `xor` (shift x amount)

randoms :: Integral a => a -> [Word32]
randoms lucky_number = map fst $ iterate (\(_, rng) -> random rng) (random $ seed lucky_number)

instance RandomGen RNGState where
  next seed_state = (first fromIntegral) $ random seed_state
    where
  genRange seed_state = (fromEnum (minBound `asTypeOf` seed_state),
                fromEnum (maxBound `asTypeOf` seed_state))

  split seed_state@(RNGState num) =  (seed_state', inverted_seed_state')
    where
      (_, seed_state') = next seed_state
      (_, inverted_seed_state') = next inverted_seed_state
      inverted_seed_state = RNGState (maxBound - num)

