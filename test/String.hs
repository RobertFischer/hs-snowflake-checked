{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module String (tests) where

import           Data.Snowchecked
import           Data.Snowchecked.Encoding.Text
import           Gens
import           Hedgehog

tests :: IO Bool
tests = checkParallel $$(discover)

prop_flakeToStringToFlake :: Property
prop_flakeToStringToFlake = property $ do
		cfg <- forAll genConfig
		flake <- forAllFlake' cfg
		let (value::Base16 String) = fromFlake flake
		let result = parseFlake cfg value
		result === Just flake
