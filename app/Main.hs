module Main where

import Unscramble
import Unscramble.Wave

main :: IO ()
main = do
  arr <- readOriginal "DATA_037.BIN"
  forEachChannel_ arr $ \ch sig -> do
    putStrLn ("Channel " ++ show ch)
    let key = recoverKey sig
    print key
    writeChannelRaw  ("ch" ++ show ch ++ "-key.bin")  key
    writeChannelWave ("ch" ++ show ch ++ "-orig.wav") sig
    writeChannelWave ("ch" ++ show ch ++ ".wav")      (applyKey key sig)
