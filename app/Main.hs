module Main where

import Control.Monad (forM_)

import Unscramble

main :: IO ()
main = do
  arr <- readOriginal "DATA_037.BIN"
  forM_ [0..7] $ \ch -> do
    print ("Channel " ++ show ch)
    let samples = channel ch arr
    key <- guessKey samples
    print (toList key)
    writeChannel ("test" ++ show ch ++ "-orig.bin")
                 samples
    writeChannel ("test" ++ show ch ++ ".bin")
                 (unscramble key samples)
