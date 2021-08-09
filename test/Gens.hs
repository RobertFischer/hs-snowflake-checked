{-# LANGUAGE TypeApplications #-}

module Gens (module Gens) where

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

genWord8 :: (MonadGen m) => m Word8
genWord8 = Gen.word8 $ Range.linear (minBound @Word8) (maxBound @Word8)

genWord256 :: (MonadGen m) => m Word256
genWord256 = Gen.integral $ Range.linear (minBound @Word256) (maxBound @Word256)

genConfig :: (MonadGen m) => m SnowcheckedConfig
genConfig = SnowcheckedConfig
		<$> Gen.integral (Range.linear 2 (maxBound @Word8))
		<*> Gen.integral (Range.linear 2 (maxBound @Word8))
		<*> Gen.integral (Range.linear 2 (maxBound @Word8))
		<*> Gen.integral (Range.linear 2 (maxBound @Word8))

forAllFlake :: (MonadIO m) => PropertyT m Flake
forAllFlake = forAll genConfig >>= forAllFlake'

forAllFlake' :: (MonadIO m) => SnowcheckedConfig -> PropertyT m Flake
forAllFlake' cfg = do
	nodeId <- forAll genWord256
	newSnowcheckedGen cfg nodeId >>= nextFlake
