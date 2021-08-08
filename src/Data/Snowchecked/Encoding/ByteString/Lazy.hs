{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans      #-}
{-|
 - This module provides a conversion function between a
 - 'Flake' and a lazy 'ByteString'.
-}

module Data.Snowchecked.Encoding.ByteString.Lazy
	( module Data.Snowchecked.Encoding.Class
	) where

import           Data.ByteString.Lazy
import           Data.Snowchecked.Encoding.Class
import           Data.Snowchecked.Encoding.Integral ()
import           Data.Snowchecked.Internal.Import
import           Prelude                            hiding (foldr)

integerToBS :: Integer -> ByteString
integerToBS = pack . mkBytes
	where
		mkBytes 0 = mempty
		mkBytes n
			= fromIntegral n
			: mkBytes (n `shiftR` 8)
{-# INLINE integerToBS #-}

bsToInteger :: ByteString -> Integer
bsToInteger = foldr mkInteger 0
	where
		mkInteger :: Word8 -> Integer -> Integer
		mkInteger nxt memo = memo `shiftL` 8 .|. toInteger nxt
{-# INLINE bsToInteger #-}

instance IsFlake ByteString where
	fromFlake = integerToBS . fromFlake
	{-# INLINE fromFlake #-}

	parseFish cfg = parseFish cfg . bsToInteger
	{-# INLINE parseFish #-}
