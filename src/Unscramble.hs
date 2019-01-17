{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Strict #-}

module Unscramble
( Sample
, readOriginal, writeChannel
, channel
, unscramble, guessKey
, toList
) where

-- The input seems to be scrambled using a xor with a repeating pattern of 64 samples.

import Control.Exception (bracket)
import Data.Array.Repa as Repa
import Data.Array.Repa.IO.Binary
import Data.Array.Repa.Repr.Unboxed (fromUnboxed, toUnboxed)
import Data.Bits
import Data.Maybe
import Data.Semigroup (Arg (..), Min (..))
import qualified Data.Vector.Unboxed as VU
import Data.Word
import Prelude as P
import qualified System.IO as IO

type Sample = Word8

readOriginal :: FilePath -> IO (Array D DIM2 Sample)
readOriginal f = do
  len <- bracket (IO.openBinaryFile f IO.ReadMode) IO.hClose IO.hFileSize
  let shape = ix2 (fromIntegral len `div` 8) 8
  transpose <$> readArrayFromStorableFile f shape

writeChannel :: Source r Sample
             => FilePath -> Array r DIM1 Sample -> IO ()
writeChannel = writeArrayToStorableFile

channel :: Source r Sample
        => Int
        -> Array r (FullShape  (Z :. Int :. All)) Sample
        -> Array D (SliceShape (Z :. Int :. All)) Sample
channel ch arr = slice arr (Z :. ch :. All)

unscramble :: (Source r0 Sample, Source r1 Sample)
           => Array r0 DIM1 Sample
           -> Array r1 DIM1 Sample
           -> Array D  DIM1 Sample
unscramble key arr = Repa.zipWith xor keyRep arr
  where
    keyRep = backpermute (extent arr) (\(Z :. n) -> (Z :. (n `mod` 64))) key

guessKey :: (Monad m, Source r Sample)
         => Array r DIM1 Sample -> m (Array U DIM1 Sample)
guessKey arr = do
  keyPreConstantXor <- keyAccum <$> keyRelative
  let Arg _ constantXor = bestConstantXor (unscramble keyPreConstantXor arr)
  pure . computeUnboxedS $ Repa.map (xor constantXor) keyPreConstantXor
  where
    -- Accumulate the relative guesses into ones relative to the first one.
    -- This key is off by a constant xor'd value.
    keyAccum k = fromUnboxed (extent k)
               . VU.postscanl' xor 0
               . toUnboxed
               $ k

    -- Each guess is relative to the previous element.
    keyRelative = computeUnboxedP $ fromFunction (ix1 64) go
      where
        go (Z :. cursor) =
          case bestKeyGuess arr cursor of
            Arg _ s -> s

-- | Given a signal xor'd with a constant value, xor it with every possible
-- value and return the one with the cleanest resulting signal.
bestConstantXor :: Source r Sample
                => Array r DIM1 Sample -> Arg Integer Sample
bestConstantXor arr = findBest (evaluateConstantXor arr)
                    $ fromListUnboxed (ix1 256) [minBound .. maxBound]

-- | Evaluate how "clean" a signal is when xor'd with a value by computing the
-- derivative of the signal and integrating its absolute value.
evaluateConstantXor :: Source r Sample
                    => Array r DIM1 Sample -> Sample -> Integer
evaluateConstantXor arr value
  = sumAllS
  . Repa.map abs
  . derivative
  . Repa.map (fromIntegral . xor value)
  $ arr

-- | Find the best guess for a key through `evaluateKeyGuess`.
bestKeyGuess :: Source r Sample
             => Array r DIM1 Sample -> Int -> Arg Integer Sample
bestKeyGuess arr cursor = findBest (evaluateKeyGuess arr cursor)
                        $ fromListUnboxed (ix1 256) [minBound .. maxBound]

-- | The sum of differences of the samples at `cursor + 64 n`
-- (xor'd with the guess) relative to the preceding ones (verbatim).
evaluateKeyGuess :: Source r Sample
                 => Array r DIM1 Sample -> Int -> Sample -> Integer
evaluateKeyGuess arr cursor guess = sumAllS diffs
  where
    diffs :: Array D DIM1 Integer
    diffs = Repa.zipWith go prevSamples currSamples
      where
        -- Assumption: a is close to (b xor guess).
        -- We only have (a xor unknown) and (b xor unknown xor guess).
        -- (a xor unknown) xor (b xor unknown xor guess) = a xor b xor guess.
        -- Given that we don't know the unknown value yet, estimate the
        -- difference based on the highest bit set in a xor b xor guess.
        go a b = bitDiff (a `xor` b `xor` guess)
          where
            bitDiff :: Word8 -> Integer
            bitDiff 0 = 0
            bitDiff x = 2 ^ (finiteBitSize x - 1 - countLeadingZeros x)

    prevSamples = backpermute (ix1 numSamples) (\(Z :. s) -> Z :. prevFirst + 64 * s) arr
    currSamples = backpermute (ix1 numSamples) (\(Z :. s) -> Z :. currFirst + 64 * s) arr

    numSamples = case (currLast - currFirst) `divMod` 64 of
      (n, 0) -> n
      r -> error $ "evaluateGuess: (currLast - currFirst) `divMod` 64 = " P.++ show r

    prevFirst
      | s >= 0    = s
      | otherwise = s + 64
      where s = cursor - 1
    currFirst = prevFirst + 1

    currLast
      | s < totalSamples = s
      | otherwise        = s - 64
      where s = (totalSamples `div` 64) * 64 + cursor

    Z :. totalSamples = extent arr

derivative :: Source r Integer
           => Array r DIM1 Integer -> Array D DIM1 Integer
derivative = adjacent subtract

adjacent :: Source r a
         => (a -> a -> a) -> Array r DIM1 a -> Array D DIM1 a
adjacent f arr
  | len > 0   = szipWith f prevArr currArr
  | otherwise = error "adjacent: Zero length"
  where
    prevArr = backpermute (Z :. (len - 1)) id                          arr
    currArr = backpermute (Z :. (len - 1)) (\(Z :. s) -> Z :. (s + 1)) arr
    Z :. len = extent arr

findBest :: (Source r a, Ord b)
         => (a -> b) -> Array r DIM1 a -> Arg b a
findBest f inputs = fromMaybe (error "findBest: Got Nothing")
                  . fmap getMin
                  . foldAllS (<>) mempty
                  $ Repa.map go inputs
  where
    go inp = Just (Min (Arg (f inp) inp))
