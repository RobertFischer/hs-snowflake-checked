{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module Integer (tests) where

import           Data.Snowchecked
import           Data.Snowchecked.Encoding.Integral
import           Gens
import           Hedgehog

tests :: IO Bool
tests = checkParallel $$(discover)

prop_flakeToIntegerToFlake :: Property
prop_flakeToIntegerToFlake = property $ do
    cfg <- forAll genConfig
    flake <- forAllFlake' cfg
    let value = fromFlake @Integer flake
    annotateShow value
    let result = parseFlake cfg value 
    result === Just flake
