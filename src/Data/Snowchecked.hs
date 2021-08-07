{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-tabs     #-}
{-|
Description : Unique id generator derived from Twitter's Snowflake.
License     : Apache 2.0
Maintainer  : smokejumperit@gmail.com
Stability   : experimental

This generates unique (guaranteed) identifiers build from a timestamp,
counter, and node id.  Identifiers are convertible to values which are
monotonically increasing with respect to time.
-}
module Data.Snowchecked
( newSnowcheckedGen
, nextFlake
, flakeToIntegral
, flakeToBase16
, flakeToByteString
, flakeToLazyByteString
, parseFlake
, SnowcheckedConfig(..)
, SnowcheckedGen
, Flake
) where

import           Control.Concurrent      (threadDelay)
import           Control.Concurrent.MVar
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Data.Bits               (Bits, shift, shiftR, (.&.), (.|.))
import qualified Data.ByteString         as SBS
import qualified Data.ByteString.Lazy    as LBS
import           Data.Snowchecked.Types
import           Data.String             (IsString, fromString)
import           Data.Time.Clock.POSIX   (getPOSIXTime)
import           Data.WideWord.Word256
import           Data.Word

-- |Converts an identifier to an integer with respect to configuration used to generate it.
flakeToIntegral :: (Num a) => Flake -> a
flakeToIntegral Flake{..} = fromIntegral
		$   (timeInteger `shift` (checkBitsInt + nodeBitsInt + countBitsInt))
		.|. (countInteger `shift` (checkBitsInt + nodeBitsInt))
		.|. (nodeIdInteger `shift` checkBitsInt)
		.|. checkInteger
	where
		SnowcheckedConfig{..} = flakeConfig
		checkBitsInt = toInt confCheckBits
		nodeBitsInt = toInt confNodeBits
		countBitsInt = toInt confCountBits
		checkInteger = cutBits (timeInteger + countInteger + nodeIdInteger) checkBitsInt
		countInteger = toInteger $ cutBits flakeCount confCountBits
		nodeIdInteger = toInteger $ cutBits flakeNodeId confNodeBits
		timeInteger = toInteger $ cutBits flakeTime confTimeBits
{-# SPECIALIZE flakeToIntegral :: Flake -> Integer #-}
{-# SPECIALIZE flakeToIntegral :: Flake -> Word64  #-}

cutBits :: (Num a, Bits a, Integral bitCount) => a -> bitCount -> a
cutBits n bits = n .&. ((1 `shift` fromIntegral bits) - 1)
{-# INLINE cutBits #-}

currentTimestamp :: IO Word256
currentTimestamp = toMillisWord256 <$> getPOSIXTime
	where
		toMillisWord256 = round . (*1000)
{-# INLINE currentTimestamp #-}

currentTimestampBits :: Word8 -> IO Word256
currentTimestampBits n = (`cutBits` fromIntegral n) <$> currentTimestamp
{-# INLINE currentTimestampBits #-}

-- | Create a new generator. Takes a configuration and node id.
newSnowcheckedGen :: (MonadIO io) => SnowcheckedConfig -> Word256 -> io SnowcheckedGen
newSnowcheckedGen conf@SnowcheckedConfig{..} nodeId = liftIO $ do
	startTimeBits <- currentTimestampBits confTimeBits
	SnowcheckedGen <$> newMVar Flake
		{ flakeTime = startTimeBits
		, flakeCount = 0
		, flakeNodeId = cutBits nodeId confNodeBits
		, flakeConfig = conf
		}
{-# INLINEABLE newSnowcheckedGen #-}
{-# SPECIALIZE newSnowcheckedGen :: SnowcheckedConfig -> Word256 -> IO SnowcheckedGen #-}

snowcheckedConfigBitCount :: SnowcheckedConfig -> Word32
snowcheckedConfigBitCount SnowcheckedConfig{..} = foldr foldFunc 0
		[ confTimeBits
		, confCountBits
		, confNodeBits
		, confCheckBits
		]
	where
		foldFunc :: Word8 -> Word32 -> Word32
		foldFunc nxt memo = memo + toWord32 nxt
{-# INLINEABLE snowcheckedConfigBitCount #-}

-- | Generates the next id.
nextFlake :: (MonadIO io) => SnowcheckedGen -> io Flake
nextFlake SnowcheckedGen{..} = liftIO $ withMVar genLastFlake mkNextFlake
	where
		mkNextFlake flake@Flake{..} =
			let SnowcheckedConfig{..} = flakeConfig in
			currentTimestampBits confTimeBits >>= \currentTimeBits ->
				if flakeTime < currentTimeBits then
					return flake
						{ flakeTime = currentTimeBits
						, flakeCount = 0
						}
				else
					let nextCount = cutBits (flakeCount + 1) confCountBits in
					if nextCount < flakeCount then
						-- The count wrapped and we need to wait for the time to change.
						-- This assumes that the next millisecond will give us a new time.
						threadDelay 1000 >> mkNextFlake flake
					else
						return flake { flakeCount = nextCount }
{-# INLINEABLE nextFlake #-}
{-# SPECIALIZE nextFlake :: SnowcheckedGen -> IO Flake #-}

-- | Provides the Flake as a base-16 string-like value. Lowercase characters are used.
flakeToBase16 :: (IsString out) => Flake -> out
flakeToBase16 flake = fromString flakeBase16Str
	where
		flakeBase16Str = consumeBytesToHex $ flakeToIntegral @Integer flake
		nextHexDigit n = case n `mod` 16 of
			10 -> 'a'
			11 -> 'b'
			12 -> 'c'
			13 -> 'd'
			14 -> 'e'
			15 -> 'f'
			x -> head $ show x
		consumeBytesToHex 0 = ""
		consumeBytesToHex n = nextHexDigit n : consumeBytesToHex (n `shiftR` 4)
{-# INLINE flakeToBase16 #-}
{-# SPECIALIZE flakeToBase16 :: Flake -> String #-}

-- | Converts a flake into a bytestring whose bytes are the bytes of the flake.
flakeToByteString :: Flake -> SBS.ByteString
flakeToByteString = LBS.toStrict . flakeToLazyByteString
{-# INLINE flakeToByteString #-}

-- | Converts a flake into a lazy bytestring whose bytes are the bytes of the flake.
flakeToLazyByteString :: Flake -> LBS.ByteString
flakeToLazyByteString = LBS.fromChunks . consumeBytes . flakeToIntegral @Integer
	where
		consumeBytes :: Integer -> [SBS.ByteString]
		consumeBytes 0 = mempty
		consumeBytes n = SBS.singleton (toWord8 n) : consumeBytes (n `shiftR` 8)
{-# INLINEABLE flakeToLazyByteString #-}

-- | Attempts to parse a flake from an integral value, ensuring that
-- the checksum is accurate based on the provided configuration.
parseFlake :: (MonadFail m, Integral a) => SnowcheckedConfig -> a -> m Flake
parseFlake cfg 0 =
	if snowcheckedConfigBitCount cfg == 0 then
		return Flake
			{ flakeTime = 0
			, flakeCount = 0
			, flakeNodeId = 0
			, flakeConfig = cfg
			}
	else
		fail $ "The value '0' is not a valid flake for configuration: " <> show cfg
parseFlake cfg@SnowcheckedConfig{..} n =
		if checksOut then
			return Flake
				{ flakeTime = fromIntegral timeInteger
				, flakeCount = fromIntegral countInteger
				, flakeNodeId = fromIntegral nodeInteger
				, flakeConfig = cfg
				}
		else
			fail $ "Checksum failed to validate for value '" <> show x <> "' and config " <> show cfg
	where
		x = toInteger n
		checkBitsInteger = toInteger confCheckBits
		nodeBitsInteger = toInteger confNodeBits
		countBitsInteger = toInteger confCountBits
		timeBitsInteger = toInteger confTimeBits
		checkInteger = cutBits x checkBitsInteger
		nodeInteger = cutBits (x `shiftR` toInt checkBitsInteger) nodeBitsInteger
		countInteger = cutBits (x `shiftR` toInt (checkBitsInteger + nodeBitsInteger)) countBitsInteger
		timeInteger = cutBits (x `shiftR` toInt (checkBitsInteger + nodeBitsInteger + countBitsInteger)) timeBitsInteger
		checksOut = checkInteger == cutBits (nodeInteger + countInteger + timeInteger) checkBitsInteger
{-# SPECIALIZE parseFlake :: (MonadFail m) => SnowcheckedConfig -> Integer -> m Flake #-}
{-# SPECIALIZE parseFlake :: (MonadFail m) => SnowcheckedConfig -> Word64 -> m Flake #-}

toInt :: (Integral a) => a -> Int
toInt = fromIntegral @_ @Int
{-# INLINE toInt #-}

toWord8 :: (Integral a) => a -> Word8
toWord8 = fromIntegral @_ @Word8
{-# INLINE toWord8 #-}

toWord32 :: (Integral a) => a -> Word32
toWord32 = fromIntegral @_ @Word32
{-# INLINE toWord32 #-}
