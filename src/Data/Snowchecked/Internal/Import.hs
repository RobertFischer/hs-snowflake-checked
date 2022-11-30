{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE TypeApplications #-}
module Data.Snowchecked.Internal.Import
	( module Data.Snowchecked.Internal.Import
	, module Data.Snowchecked.Types
	, module Data.Bits
	, module Data.WideWord.Word256
	, module Data.Word
	, module Numeric
	) where

import           Data.Bits              (Bits, shiftL, shiftR, (.&.), (.|.))
import           Data.Snowchecked.Types
import           Data.WideWord.Word256
import           Data.Word
import           Numeric

cutBits :: (Num a, Bits a) => a -> Int -> a
cutBits n bits = n .&. ((1 `shiftL` bits) - 1)
{-# INLINE cutBits #-}

cutShiftBits :: (Num a, Bits a) => a -> Int -> Int -> a
cutShiftBits n cutBitCount shiftBitCount = cutBits n cutBitCount `shiftL` shiftBitCount
{-# INLINE cutShiftBits #-}

shiftCutBits :: (Num a, Bits a) => a -> Int -> Int -> a
shiftCutBits n shiftBitCount = cutBits $ n `shiftR` shiftBitCount
{-# INLINE shiftCutBits #-}

toInt :: (Integral a) => a -> Int
toInt = fromIntegral @_ @Int
{-# INLINE toInt #-}

toWord8 :: (Integral a) => a -> Word8
toWord8 = fromIntegral @_ @Word8
{-# INLINE toWord8 #-}

toWord32 :: (Integral a) => a -> Word32
toWord32 = fromIntegral @_ @Word32
{-# INLINE toWord32 #-}

