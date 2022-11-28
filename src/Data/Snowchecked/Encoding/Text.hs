{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans      #-}
{-|
 This module provides a generalized conversion function between a
 'Flake' and all types that are members of both 'FromText' and 'ToText'.
 It is specialized for the strict 'Text' and 'String' types. It is marked as
 incoherent due to the constraint being no smaller than the instance type,
 so it is undecidable.

 To specify how you want the conversion to be performed, you need to wrap the
 text-like type the 'Base16' constructor.  Other encodings (eg: Base64) may
 be added later.

 Note that when converting to a 'Flake', the implementation silently discards
 characters other than digits, 'a'-'f', and 'A'-'F'.  This allows you to
 apply formatting to the Flake.
-}

module Data.Snowchecked.Encoding.Text
	( module Data.Snowchecked.Encoding.Class
	, module Data.Text.Conversions
	) where

import           Control.Applicative                       ((<|>))
import qualified Data.ByteString.Lazy                      as LBS
import qualified Data.List                                 as L
import           Data.Maybe                                (catMaybes)
import           Data.Snowchecked.Encoding.ByteString.Lazy ()
import           Data.Snowchecked.Encoding.Class
import           Data.Snowchecked.Internal.Import
import qualified Data.Text                                 as T
import           Data.Text.Conversions
import           Text.Read                                 (readMaybe)

-- | Convert a hex value to a character.
--
--   WARNING: This function returns the null character ('\0') if you pass in a value > 15.
c :: Word8 -> Char
c 0  = '0'
c 1  = '1'
c 2  = '2'
c 3  = '3'
c 4  = '4'
c 5  = '5'
c 6  = '6'
c 7  = '7'
c 8  = '8'
c 9  = '9'
c 10 = 'a'
c 11 = 'b'
c 12 = 'c'
c 13 = 'd'
c 14 = 'e'
c 15 = 'f'
c _  = '\0'
{-# INLINE c #-}

-- | Converts a character to a hex value (if there is one).
b :: Char -> Maybe Word8
b ch = readMaybe [ch] <|> chLookup
	where
		chLookup = case ch of
			'A' -> Just 10
			'a' -> Just 10
			'B' -> Just 11
			'b' -> Just 11
			'C' -> Just 12
			'c' -> Just 12
			'D' -> Just 13
			'd' -> Just 13
			'E' -> Just 14
			'e' -> Just 14
			'F' -> Just 15
			'f' -> Just 15
			_   -> Nothing
{-# INLINE b #-}

-- | Converts a byte to two hex characters: low nibble and then high nibble.
byteToHex :: Word8 -> (Char,Char)
byteToHex w8 = (c lowNibble, c highNibble)
	where
		lowNibble = cutBits w8 4
		highNibble = shiftCutBits w8 4 4
{-# INLINE byteToHex #-}

instance {-# INCOHERENT #-} (ToText a, FromText a) => IsFlake (Base16 a) where
	fromFlake flake = Base16 $ convertText str
		where
			str = LBS.foldr bytesToChars [] $ fromFlake flake
			bytesToChars w8 rest =
				let (lowC, highC) = byteToHex w8 in lowC : highC : rest
	{-# INLINEABLE fromFlake #-}
	{-# SPECIALIZE fromFlake :: Flake -> Base16 String #-}
	{-# SPECIALIZE fromFlake :: Flake -> Base16 T.Text #-}

	parseFish SnowcheckedConfig{..} (Base16 raw) = return $ Flakeish
			{ fishCheck = fromIntegral $ cutBits n checkBitsInt
			, fishNodeId = fromIntegral $ shiftCutBits n checkBitsInt nodeBitsInt
			, fishCount = fromIntegral $ shiftCutBits n (checkBitsInt + nodeBitsInt) countBitsInt
			, fishTime = fromIntegral $ shiftCutBits n (checkBitsInt + nodeBitsInt + countBitsInt) timeBitsInt
			}
		where
			nibbles = catMaybes . T.foldr toNibbles [] $ toText raw
			n = L.foldr addNibbles 0 nibbles
			addNibbles nib total = toInteger nib + ( total `shiftL` 4 )
			toNibbles ch lst = b ch : lst
			checkBitsInt = toInt confCheckBits
			nodeBitsInt = toInt confNodeBits
			timeBitsInt = toInt confTimeBits
			countBitsInt = toInt confCountBits
	{-# INLINEABLE parseFish #-}
	{-# SPECIALIZE parseFish :: (MonadFail m) => SnowcheckedConfig -> Base16 T.Text -> m Flakeish #-}
	{-# SPECIALIZE parseFish :: (MonadFail m) => SnowcheckedConfig -> Base16 String -> m Flakeish #-}
