{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}

module Unscramble
( Sample
, readOriginal, writeChannelRaw, writeChannelRawHandle
, forEachChannel, forEachChannel_
, recoverKey, applyKey
) where

-- The input seems to be scrambled using a xor with a repeating pattern of 64 samples.

import Control.Exception (evaluate)
import Data.Massiv.Array as A
import Data.Massiv.Array.Manifest.Vector
import Data.Bits
import Data.Foldable as F
import Data.List (foldl')
import Data.Maybe
import Data.Semigroup (Arg (..), Min (..))
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.MMap as VS
import qualified Data.Vector.Unboxed as VU
import Data.Word
import Foreign.Storable
import Prelude as P
import qualified System.IO as IO

type Sample = Word8

readOriginal :: FilePath -> IO (Array M Ix2 Sample)
readOriginal path = do
  unsafeVec <- VS.unsafeMMapVector path Nothing
  let elems = VS.length unsafeVec `div` sizeOf (undefined :: Sample)
      sz    = Ix2 (elems `div` 8) 8

      unsafeArr = fromVector Seq sz unsafeVec :: Array S Ix2 Sample
      -- Transpose from an [n][8] to an [8][n] array.
      unsafeArrTransp = transposeInner unsafeArr
  -- Make a copy, so we no longer depend on mmapped data. Also, the data will
  -- be in a more cache-friendly form due to the transposition.
  let !arr = toManifest $ computeAs S unsafeArrTransp
  evaluate arr

writeChannelRaw :: Manifest r Ix1 Sample
                => FilePath -> Array r Ix1 Sample -> IO ()
writeChannelRaw path arr =
  IO.withBinaryFile path IO.WriteMode (`writeChannelRawHandle` arr)

writeChannelRawHandle :: Manifest r Ix1 Sample
                      => IO.Handle -> Array r Ix1 Sample -> IO ()
writeChannelRawHandle h arr = VS.unsafeWith vec $ \ptr -> IO.hPutBuf h ptr bytes
  where
    vec   = toVector (convertAs S arr)  -- TODO: Avoid a copy.
    bytes = VS.length vec * sizeOf (undefined :: Sample)

forEachChannel :: Applicative f
               => Array M Ix2 Sample
               -> (Int -> Array M Ix1 Sample -> f a)
               -> f [a]
forEachChannel sig f = P.traverse (\ch -> f ch (sig !> ch)) $ take (outerLength sig) [0..]

forEachChannel_ :: Applicative f
                => Array M Ix2 Sample
                -> (Int -> Array M Ix1 Sample -> f a)
                -> f ()
forEachChannel_ sig f = F.traverse_ (\ch -> f ch (sig !> ch)) $ take (outerLength sig) [0..]

recoverKey :: Array M Ix1 Sample  -- ^ Encrypted signal
           -> Array U Ix1 Sample  -- ^ Recovered key
recoverKey sig = keyWith $ minimize Par 256 fromIntegral evaluateConstant
  where
    evaluateConstant :: Sample   -- ^ A guess for constant
                     -> Integer  -- ^ A loss estimate
    evaluateConstant guess
      = A.sum . A.map abs . derivative . A.map fromIntegral
      $ applyKeyD (keyWith guess) sig

    -- | The key is a xor "integral" of Δkey, thus it's floating on
    -- top of some constant. Compute the key given the constant.
    keyWith :: Sample -> Array U Ix1 Sample
    keyWith constant = fromVector Seq (size dKey)
                     . VU.postscanl' xor constant
                     . toVector
                     $ dKey

    dKey = makeArrayR U Seq 64 (`recoverDKeyAt` sig)

derivative :: (Source r Ix1 a, Source (EltRepr r Ix1) Ix1 a, Num a)
           => Array r Ix1 a -> Array D Ix1 a
derivative sig = A.zipWith subtract sig sigTail
  where
    sigTail = extract' 1 (size sig - 1) sig

applyKey :: Array U Ix1 Sample  -- ^ Key
         -> Array M Ix1 Sample  -- ^ Encrypted signal
         -> Array M Ix1 Sample  -- ^ Decrypted signal
applyKey key sig = toManifest . computeAs S $ applyKeyD key sig

applyKeyD :: Array U Ix1 Sample  -- ^ Key
          -> Array M Ix1 Sample  -- ^ Encrypted signal
          -> Array D Ix1 Sample  -- ^ Decrypted signal
applyKeyD key sig = setComp Seq $ A.zipWith xor keyRepeated sig
  where
    keyRepeated = backpermute (size sig) (`mod` 64) key

recoverDKeyAt :: Ix1                 -- ^ Key index (0…63)
              -> Array M Ix1 Sample  -- ^ Encrypted signal
              -> Sample              -- ^ Estimated Δkey[k]
recoverDKeyAt cursor sig = minimize Par 256 fromIntegral evaluateGuess
  where
    evaluateGuess :: Sample   -- ^ A guess for Δkey[k] (0…255)
                  -> Integer  -- ^ A loss estimate
    evaluateGuess guess
      = A.sum . setComp Seq
      $ A.zipWith (\a b -> fromIntegral (msbValue (a `xor` b `xor` guess)))
                  prevSamples currSamples

    -- Samples at [64n+cursor-1] for all n.
    prevSamples = backpermute samplesSz (\n -> prevFirst + 64*n) sig
    -- Samples at [64n+cursor] for all n.
    currSamples = backpermute samplesSz (\n -> currFirst + 64*n) sig

    ~(samplesSz, 0) = (currLast - currFirst) `divMod` 64

    -- The first [64n+cursor-1] position the signal.
    prevFirst = validateIx "evaluateDKeyAt/prevFirst" (size sig)
              $ repairIndex (size sig) (cursor - 1)
                            (const (+64)) (const (subtract 64))
    -- The first [64n+cursor] position following prevFirst.
    currFirst = validateIx "evaluteDKeyAt/currFirst" (size sig) (prevFirst + 1)

    -- The last [64n+cursor] position the signal.
    currLast  = validateIx "evaluateDKeyAt/currLast" (size sig)
              $ repairIndex (size sig) ((size sig `div` 64) * 64 + cursor)
                            (const (+64)) (const (subtract 64))

validateIx :: Index ix => String -> ix -> ix -> ix
validateIx name sz ix
  | isSafeIndex sz ix = ix
  | otherwise = errorIx name sz ix
{-# INLINE validateIx #-}

minimize :: Comp -> Ix1 -> (Ix1 -> a) -> (a -> Integer) -> a
minimize comp sz fromIx evalLoss =
  case ifoldMono (\ix loss -> Just (Min (Arg loss (fromIx ix)))) losses of
    Just (Min (Arg _ value)) -> value
    Nothing -> error "minimize: empty array"
  where
    losses :: Array D Ix1 Integer
    losses = makeArrayR D comp sz (evalLoss . fromIx)

-- | Estimate the magnitude of an unsigned integer by its most
-- significant set bit. Knowing an `a xor z` and a `b xor z`,
-- this enables one to roughly estimate the absolute difference
-- between `a` and `b` without knowing `z` because
-- `msbValue ((a xor z) `xor` (b xor z)) == msbValue (a xor b)`.
msbValue :: Word8 -> Word8
msbValue n = n .&. complement mask
  where mask = (foldl' (.|.) 0 . P.map (n `shiftR`)) [1 .. finiteBitSize n - 1]
{-# INLINE msbValue #-}
