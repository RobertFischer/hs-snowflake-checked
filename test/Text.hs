{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module Text (tests) where

import           Data.Snowchecked
import           Data.Snowchecked.Encoding.Text
import           Data.Text                      (Text)
import           Gens
import           Hedgehog

tests :: IO Bool
tests = checkParallel $$(discover)

prop_flakeToStrictTextToFlake :: Property
prop_flakeToStrictTextToFlake = property $ do
		cfg <- forAll genConfig
		flake <- forAllFlake' cfg
		let (value::Base16 Text) = fromFlake flake
		let result = parseFlake cfg value
		result === Just flake
