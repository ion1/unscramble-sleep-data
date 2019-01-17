{-# LANGUAGE FlexibleContexts #-}

module Unscramble.Wave
( writeChannelWave
) where

import Codec.Audio.Wave
import Data.Array.Repa as Repa
import Data.Array.Repa.Eval (Load)
import Data.Array.Repa.Repr.ForeignPtr
import Foreign.ForeignPtr
import Foreign.Storable
import System.IO

import Unscramble

writeChannelWave :: (Source r Sample, Load r DIM1 Sample)
                 => FilePath -> Array r DIM1 Sample -> IO ()
writeChannelWave f arr = do
  writeWaveFile f wave put
  where
    Z :. samples = extent arr
    bytes = samples * sizeOf (arr ! zeroDim)

    put handle = do
      fp <- mallocForeignPtrBytes bytes
      computeIntoP fp arr
      withForeignPtr fp $ \p -> hPutBuf handle p bytes

    wave = Wave{ waveFileFormat   = WaveVanilla
               , waveSampleRate   = 10
               , waveSampleFormat = SampleFormatPcmInt 8
               , waveChannelMask  = speakerMono
               , waveDataOffset   = 0
               , waveDataSize     = 0
               , waveSamplesTotal = 0
               , waveOtherChunks  = []
               }
