{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module Word64 (tests) where

import           Control.Monad
import           Data.Snowchecked
import           Data.Snowchecked.Encoding.Integral
import           Data.Word
import           Gens
import           Hedgehog

tests :: IO Bool
tests = checkParallel $$(discover)

prop_flakeToWord64ToFlake :: Property
prop_flakeToWord64ToFlake = withDiscards (999999999 :: DiscardLimit) . property $ do
		cfg <- forAll genConfig
		unless (snowcheckedConfigBitCount cfg <= 64) discard
		flake <- forAllFlake' cfg
		let value = fromFlake @Word64 flake
		let result = parseFlake cfg value
		result === Just flake
