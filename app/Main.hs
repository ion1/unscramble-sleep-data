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
    writeChannelRaw  ("ch" ++ show ch ++ "-key.bin")  key
    writeChannelWave ("ch" ++ show ch ++ "-orig.wav") samples
    writeChannelWave ("ch" ++ show ch ++ ".wav")      (unscramble key samples)
