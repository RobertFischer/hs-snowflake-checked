{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans      #-}
{-|
 This module provides a conversion function between a
 'Flake' and a strict 'ByteString'.  The 'ByteString'
 is the series of bytes that make up the 'Flake', with
 the lower bytes being in the lower indecies.
-}

module Data.Snowchecked.Encoding.ByteString
	( module Data.Snowchecked.Encoding.Class
	) where

import           Data.ByteString
import           Data.Snowchecked.Encoding.ByteString.Lazy ()
import           Data.Snowchecked.Encoding.Class

instance IsFlake ByteString where
	fromFlake = toStrict . fromFlake
	{-# INLINE fromFlake #-}

	parseFish cfg bs = parseFish cfg $ fromStrict bs
	{-# INLINE parseFish #-}
