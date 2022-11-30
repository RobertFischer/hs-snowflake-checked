{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module String (module String) where

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
    annotateFlakeInteger flake
    let value = fromFlake @(Base16 String) flake
    annotateShow value
    let (result::Either String Flake) = parseFlake cfg value
    result === Right flake
