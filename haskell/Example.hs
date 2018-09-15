module Main where
import Data.Word (Word32)
import Control.Monad
import System.Random
import System.IO

import qualified SimpleRNG

main :: IO ()
main = do
  let lucky_number = 42 :: Integer
  Control.Monad.forM_ (take 10 $ SimpleRNG.randoms lucky_number) (System.IO.putStrLn . show)

