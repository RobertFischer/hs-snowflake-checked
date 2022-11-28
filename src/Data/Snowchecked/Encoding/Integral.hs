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
			$   cutBits checkInteger checkBitsInt
			.|. cutShiftBits nodeIdInteger nodeBitsInt checkBitsInt
			.|. cutShiftBits countInteger countBitsInt (checkBitsInt + nodeBitsInt)
			.|. cutShiftBits timeInteger timeBitsInt (checkBitsInt + nodeBitsInt + countBitsInt)
		where
			SnowcheckedConfig{..} = flakeConfig
			checkBitsInt = toInt confCheckBits
			nodeBitsInt = toInt confNodeBits
			timeBitsInt = toInt confTimeBits
			countBitsInt = toInt confCountBits
			nodeIdInteger = toInteger flakeNodeId
			timeInteger = toInteger flakeTime
			countInteger = toInteger flakeCount
			checkInteger = nodeIdInteger + timeInteger + countInteger
	{-# INLINEABLE fromFlake #-}

	parseFish SnowcheckedConfig{..} i = return $ Flakeish
			{ fishCheck = fromIntegral $ cutBits n checkBitsInt
			, fishNodeId = fromIntegral $ shiftCutBits n checkBitsInt nodeBitsInt
			, fishCount = fromIntegral $ shiftCutBits n (checkBitsInt + nodeBitsInt) countBitsInt
			, fishTime = fromIntegral $ shiftCutBits n (checkBitsInt + nodeBitsInt + countBitsInt) timeBitsInt
			}
		where
			n = toInteger i
			checkBitsInt = toInt confCheckBits
			nodeBitsInt = toInt confNodeBits
			timeBitsInt = toInt confTimeBits
			countBitsInt = toInt confCountBits
	{-# INLINE parseFish #-}
