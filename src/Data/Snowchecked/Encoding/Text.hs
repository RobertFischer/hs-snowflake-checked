{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns       #-}
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

import qualified Data.List                                 as L
import           Data.Maybe                                (fromMaybe)
import           Data.Snowchecked.Encoding.Integral
import           Data.Snowchecked.Encoding.Class
import           Data.Snowchecked.Internal.Import
import qualified Data.Text                                 as T
import           Data.Text.Conversions
import Data.Snowchecked (snowcheckedConfigBitCount)
import Data.Ratio ((%))
import Data.Char (isHexDigit)

instance {-# INCOHERENT #-} (ToText a, FromText a) => IsFlake (Base16 a) where
  fromFlake flake@Flake{flakeConfig} = Base16 $ convertText str
    where
      hexLength = ceiling $
        snowcheckedConfigBitCount flakeConfig % 4
      pad0 str' = 
        if L.length str' < hexLength then
          pad0 ('0':str')
        else
          str'
      str = pad0 $ showHex (fromFlake @Integer flake) ""
  {-# INLINEABLE fromFlake #-}
  {-# SPECIALIZE fromFlake :: Flake -> Base16 String #-}
  {-# SPECIALIZE fromFlake :: Flake -> Base16 T.Text #-}

  parseFish SnowcheckedConfig{..} (Base16 raw) = 
    calculateN >>= \n ->
      return $ Flakeish
        { fishCheck = fromIntegral $ cutBits n checkBitsInt
        , fishNodeId = fromIntegral $ shiftCutBits n checkBitsInt nodeBitsInt
        , fishCount = fromIntegral $ shiftCutBits n (checkBitsInt + nodeBitsInt) countBitsInt
        , fishTime = fromIntegral $ shiftCutBits n (checkBitsInt + nodeBitsInt + countBitsInt) timeBitsInt
        }
    where
      str = convertText @_ @String raw
      cleaned = 
        L.dropWhile ('0' ==) .
        L.filter isHexDigit $
        fromMaybe str (L.stripPrefix "0x" str)
      calculateN = fst <$> findBestResult (readHex @Integer cleaned)
      findBestResult [] = fail "Could not find any results"
      findBestResult (this@(_,""):_) = return this
      findBestResult [onlyResult] = return onlyResult
      findBestResult (this@(_,nRest):others) =
        findBestResult others >>= \other@(_, mRest) ->
          if L.length nRest < L.length mRest then
            return this
          else
            return other
      checkBitsInt = toInt confCheckBits
      nodeBitsInt = toInt confNodeBits
      timeBitsInt = toInt confTimeBits
      countBitsInt = toInt confCountBits
  {-# INLINEABLE parseFish #-}
  {-# SPECIALIZE parseFish :: (MonadFail m) => SnowcheckedConfig -> Base16 T.Text -> m Flakeish #-}
  {-# SPECIALIZE parseFish :: (MonadFail m) => SnowcheckedConfig -> Base16 String -> m Flakeish #-}
