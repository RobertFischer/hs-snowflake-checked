{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module Text (module Text) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Snowchecked
import           Data.Snowchecked.Encoding.Text
import           Data.Text                      (Text)
import           Gens
import           Hedgehog
import Data.Bits (shiftR)
import Data.Ratio ( (%) )
import qualified Data.Text as T
import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range

tests :: IO Bool
tests = checkParallel $$(discover)

prop_flakeToStrictTextToFlake :: Property
prop_flakeToStrictTextToFlake = property $ do
    cfg <- forAll genConfig
    flake <- forAllFlake' cfg
    annotateFlakeInteger flake
    let value = fromFlake @(Base16 Text) flake
    annotateShow value
    let result = parseFlake cfg value
    result === Just flake

prop_flakeToStrictTextHasRightLength :: Property
prop_flakeToStrictTextHasRightLength = property $ do
    cfg <- forAll genConfig
    flake <- forAllFlake' cfg
    annotateFlakeInteger flake
    let Base16 value = fromFlake @(Base16 Text) flake
    T.length value === hexDigitCount (snowcheckedConfigBitCount cfg)
  where
    hexDigitCount = ceiling . ( % 4 )

prop_monotonicallyIncreasing :: Property
prop_monotonicallyIncreasing = property $ do
    cfg <- forAll genConfig
    let flakeCount = min 1024 (fromIntegral $ uniqueFlakeCount cfg)
    lst <- forAll $ Gen.list (Range.linear 2 flakeCount) (return ())
    nodeId <- forAll genWord256
    flakeGen <- newSnowcheckedGen cfg nodeId
    resultLst <- mapM (\_ -> nextFlake flakeGen) lst
    checkResults $ unBase16 . fromFlake @(Base16 Text) <$> resultLst
  where
    checkResults [] = success
    checkResults [_] = success
    checkResults (a:b:rest) = do
      diff a (<) b
      checkResults (b:rest)
