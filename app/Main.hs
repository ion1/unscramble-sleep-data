module Main where

import Control.Monad (forM_)

import Unscramble
import Unscramble.Wave

main :: IO ()
main = do
  arr <- readOriginal "DATA_037.BIN"
  forM_ [0..7] $ \ch -> do
    print ("Channel " ++ show ch)
    let samples = channel ch arr
    key <- guessKey samples
    print (toList key)
    writeChannelWave ("test" ++ show ch ++ "-orig.wav")
                     samples
    writeChannelWave ("test" ++ show ch ++ ".wav")
                     (unscramble key samples)
