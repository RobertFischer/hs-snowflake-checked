{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}

module Gens (module Gens) where

import           Data.Snowchecked.Encoding.Text
import           Data.Snowchecked.Encoding.Integral
import           Data.Text                      (Text)
import           Control.Monad          (unless, void)
import           Control.Monad.IO.Class (MonadIO)
import           Data.List              (nub)
import           Data.Snowchecked
import           Data.WideWord.Word256
import           Data.Word
import           Hedgehog
import qualified Hedgehog.Gen           as Gen
import           Hedgehog.Main          (defaultMain)
import qualified Hedgehog.Range         as Range

genBytes :: (MonadGen m) => m [Word8]
genBytes = strip0 <$>
    Gen.list 
      (Range.linear 1 (fromIntegral $ maxBound @Word8))
      genWord8
  where
    strip0 lst = 
      case lst of
        [] -> [0]
        [0] -> [0]
        (0:rest) -> strip0 rest
        _ -> lst

genWord8 :: (MonadGen m) => m Word8
genWord8 = Gen.word8 $ Range.linear (minBound @Word8) (maxBound @Word8)

genWord256 :: (MonadGen m) => m Word256
genWord256 = Gen.integral $ Range.linear (minBound @Word256) (maxBound @Word256)

genConfig :: (MonadGen m) => m SnowcheckedConfig
genConfig = SnowcheckedConfig
		<$> Gen.integral (Range.linear 4 (maxBound @Word8))
		<*> Gen.integral (Range.linear 0 (maxBound @Word8))
		<*> Gen.integral (Range.linear 0 (maxBound @Word8))
		<*> Gen.integral (Range.linear 0 (maxBound @Word8))

forAllFlake :: (MonadIO m) => PropertyT m Flake
forAllFlake = forAll genConfig >>= forAllFlake'

forAllFlake' :: (MonadIO m) => SnowcheckedConfig -> PropertyT m Flake
forAllFlake' cfg = do
	nodeId <- forAll genWord256
	newSnowcheckedGen cfg nodeId >>= nextFlake

annotateFlakeString :: (MonadTest m) => Flake -> m ()
annotateFlakeString = annotateShow . fromFlake @(Base16 String)

annotateFlakeText :: (MonadTest m) => Flake -> m ()
annotateFlakeText = annotateShow . fromFlake @(Base16 Text)

annotateFlakeInteger :: (MonadTest m) => Flake -> m ()
annotateFlakeInteger = annotateShow . fromFlake @Integer

instance MonadFail (Either String) where
  fail = Left
