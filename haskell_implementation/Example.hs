module Main where
import Data.Word (Word32)
import Control.Monad
import System.Random
import System.IO

import SimpleRNG

deterministic_stream :: Integral a => a -> [Word32]
deterministic_stream lucky_number = map fst $ iterate (\(_, rng) -> rand_r rng) (rand_r $ seed lucky_number)

main :: IO ()
main = do
  let lucky_number = 42 :: Integer
  Control.Monad.forM_ (take 10 $ deterministic_stream lucky_number) (System.IO.putStrLn . show)

{-| Using this version would be nice,
but currently the RandomGen wrapper of SimpleRNG will not result in the same random numbers as the C, Java and Python libraries.
-}
main_broken :: IO ()
main_broken = do
  let lucky_number = 42 :: Word32
  let rng = (SimpleRNG.seed lucky_number)
  let random_numbers = System.Random.randoms rng :: [Word32]

  Control.Monad.forM_ (take 10 random_numbers) (System.IO.putStrLn . show)
