{-# LANGUAGE DeriveGeneric #-}
{-|
Description : Types for the Data.Snowchecked module
License     : Apache 2.0
Maintainer  : smokejumperit@gmail.com
Stability   : experimental

This module is internal, subject to breaking changes without a major version increment,
and therefore should only be imported by Data.Snowchecked.
-}
module Data.Snowchecked.Types
	( module Data.Snowchecked.Types )
	where

import           Control.Concurrent.MVar
import           Control.DeepSeq         (NFData)
import           Data.Default
import           Data.WideWord.Word256
import           Data.Word
import           GHC.Generics            (Generic)

{-|
Configuration that specifies how many bits are used for each part of the id.
These values are not validated and may be any legal value for the type.

The default value provided by 'def' is 64 bits in total length, just like
the original Snowflake algorithm.

Note that specifying 0 check bits results in the normal Snowflake generation.
Setting 'confTimeBits' or 'confCountBits' near zero will significantly limit
the total number of UIDs that can be generated, as well as the throughput of
the UID generation.
-}
data SnowcheckedConfig = SnowcheckedConfig
	{ confTimeBits         :: Word8  -- ^ Number of bits used to hold the time
	, confCountBits        :: Word8  -- ^ Number of bits used to count instances per-time
	, confNodeBits         :: Word8  -- ^ Number of bits derived from the node id
	, confCheckBits        :: Word8  -- ^ Number of bits used to store the checksum
	} deriving (Eq, Show, Generic)

instance NFData SnowcheckedConfig

instance Default SnowcheckedConfig where
	{-|
		A configuration using 40 bits for time, 10 bits for count, 8 bits for node id,
		and 6 bits for the checksum.
	-}
	def = SnowcheckedConfig
		{ confTimeBits = 40
		, confCountBits = 10
		, confNodeBits = 8
		, confCheckBits = 6
		}

{-|
	The state that needs to be communicated between flake generation calls.
	This should be treated as opaque by consumers of this library: messing
	about with its internals may cause your code to hang indefinitely.
 -}
newtype SnowcheckedGen = SnowcheckedGen { genLastFlake :: MVar Flake }

{-| The state of a given generated instance. Note that the actual value is calculated on demand. -}
data Flake = Flake
	{ flakeTime          :: Word256 -- ^ The bit-truncated time
	, flakeCount         :: Word256 -- ^ The bit-truncated count
	, flakeNodeId        :: Word256 -- ^ The bit-truncated node id
	, flakeConfig        :: SnowcheckedConfig -- ^ The configuration used to create the flake.
 } deriving (Eq,Show,Generic)

instance NFData Flake
