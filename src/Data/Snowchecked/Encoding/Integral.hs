{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans      #-}
{-|
 This module provides a generalized conversion function between a
 'Flake' and all members of the typeclass 'Integral'.  It is specialized
 for the 'Integer', 'Word32', and 'Word64' types. It is marked as
 incoherent due to the constraint being no smaller than the instance type,
 so it is undecidable.
-}

module Data.Snowchecked.Encoding.Integral
	( module Data.Snowchecked.Encoding.Class
	) where

import           Data.Snowchecked.Encoding.Class
import           Data.Snowchecked.Internal.Import

instance {-# INCOHERENT #-} (Integral a) => IsFlake a where
	fromFlake Flake{..} = fromInteger
			$   cutBits checkInteger checkBitsInteger
			.|. cutShiftBits nodeIdInteger nodeBitsInteger checkBitsInteger
			.|. cutShiftBits countInteger countBitsInteger (checkBitsInteger + nodeBitsInteger)
			.|. cutShiftBits timeInteger timeBitsInteger (checkBitsInteger + nodeBitsInteger + countBitsInteger)
		where
			SnowcheckedConfig{..} = flakeConfig
			checkBitsInteger = toInteger confCheckBits
			nodeBitsInteger = toInteger confNodeBits
			timeBitsInteger = toInteger confTimeBits
			countBitsInteger = toInteger confCountBits
			nodeIdInteger = toInteger flakeNodeId
			timeInteger = toInteger flakeTime
			countInteger = toInteger flakeCount
			checkInteger = nodeIdInteger + timeInteger + countInteger
	{-# INLINEABLE fromFlake #-}

	parseFish SnowcheckedConfig{..} i = return $ Flakeish
			{ fishCheck = fromIntegral $ cutBits n checkBitsInteger
			, fishNodeId = fromIntegral $ shiftCutBits n checkBitsInteger nodeBitsInteger
			, fishCount = fromIntegral $ shiftCutBits n (checkBitsInteger + nodeBitsInteger) countBitsInteger
			, fishTime = fromIntegral $ shiftCutBits n (checkBitsInteger + nodeBitsInteger + countBitsInteger) timeBitsInteger
			}
		where
			n = toInteger i
			checkBitsInteger = toInteger confCheckBits
			nodeBitsInteger = toInteger confNodeBits
			timeBitsInteger = toInteger confTimeBits
			countBitsInteger = toInteger confCountBits
	{-# INLINE parseFish #-}
