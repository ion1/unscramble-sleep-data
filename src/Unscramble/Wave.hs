{-# LANGUAGE FlexibleContexts #-}

module Unscramble.Wave
( writeChannelWave
) where

import Codec.Audio.Wave
import Data.Massiv.Array as A

import Unscramble

writeChannelWave :: FilePath -> Array M Ix1 Sample -> IO ()
writeChannelWave path arr = do
  writeWaveFile path wave (`writeChannelRawHandle` arr)
  where
    wave = Wave{ waveFileFormat   = WaveVanilla
               , waveSampleRate   = 10
               , waveSampleFormat = SampleFormatPcmInt 8
               , waveChannelMask  = speakerMono
               , waveDataOffset   = 0
               , waveDataSize     = 0
               , waveSamplesTotal = 0
               , waveOtherChunks  = []
               }
