{-# LANGUAGE RecordWildCards #-}

module Data.Snowchecked.Encoding.Class ( IsFlake(..), Flakeish(..), goodFish ) where

import           Data.Snowchecked.Internal.Import

{-| Something that might be a 'Flake'.  The fields might not be truncated
 - to the appropriate size.
 -}
data Flakeish = Flakeish
	{ fishNodeId    ::  Word256
	, fishCount     ::  Word256
	, fishTime      ::  Word256
	, fishCheck     ::  Word256
	}

{-| Is this 'Flakeish' valid under the given 'SnowcheckedConfig' settings? -}
goodFish :: SnowcheckedConfig -> Flakeish -> Bool
goodFish SnowcheckedConfig{..} Flakeish{..} =
		checkInteger == cutBits (nodeInteger + countInteger + timeInteger) confCheckBits
	where
		checkInteger = cutBits fishCheck confCheckBits
		nodeInteger = cutBits fishNodeId confNodeBits
		countInteger = cutBits fishCount confCountBits
		timeInteger = cutBits fishTime confTimeBits
{-# INLINEABLE goodFish #-}

{-| The class of things that can be generated from and to a 'Flake'.
 -}
class IsFlake a where
	{-# MINIMAL fromFlake, (parseFish | parseFlake) #-}
	fromFlake :: Flake -> a
	parseFlake :: (MonadFail m) => SnowcheckedConfig -> a -> m Flake
	parseFlake cfg@SnowcheckedConfig{..} a = parseFish cfg a >>= \fish@Flakeish{..} ->
		if goodFish cfg fish then
			return $ Flake
				{ flakeTime = cutBits fishTime confTimeBits
				, flakeCount = cutBits fishCount confCountBits
				, flakeNodeId = cutBits fishNodeId confNodeBits
				, flakeConfig = cfg
				}
		else
			fail "Checksum is incorrect for Snowchecked flake"

	parseFish :: (MonadFail m) => SnowcheckedConfig -> a -> m Flakeish
	parseFish cfg a = toFlakeish <$> parseFlake cfg a
		where
			toFlakeish Flake{..} = Flakeish
				{ fishTime = flakeTime
				, fishCount = flakeCount
				, fishNodeId = flakeNodeId
				, fishCheck = flakeTime + flakeCount + flakeNodeId
				}
