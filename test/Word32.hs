{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module Word32 (tests) where

import           Control.Monad
import           Data.Snowchecked
import           Data.Snowchecked.Encoding.Integral
import           Data.Word
import           Gens
import           Hedgehog
import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range

tests :: IO Bool
tests = checkParallel $$(discover)

prop_flakeToWord32ToFlake :: Property
prop_flakeToWord32ToFlake = withDiscards (999999999 :: DiscardLimit) . property $ do
    cfg <- forAll genConfig
    unless (snowcheckedConfigBitCount cfg <= 32) discard
    flake <- forAllFlake' cfg
    let value = fromFlake @Word32 flake
    let result = parseFlake cfg value
    result === Just flake

prop_monotonicallyIncreasing :: Property
prop_monotonicallyIncreasing = withDiscards (999999999 :: DiscardLimit) . property $ do
    cfg <- forAll genConfig
    unless (snowcheckedConfigBitCount cfg <= 32) discard
    unless (confTimeBits cfg >= 16) discard
    let flakeCount = min 1024 (fromIntegral $ uniqueFlakeCount cfg)
    lst <- forAll $ Gen.list (Range.linear 2 flakeCount) (return ())
    nodeId <- forAll genWord256
    flakeGen <- newSnowcheckedGen cfg nodeId
    resultLst <- mapM (\_ -> nextFlake flakeGen) lst
    checkResults $ fromFlake @Word32 <$> resultLst
  where
    checkResults [] = success
    checkResults [_] = success
    checkResults (a:b:rest) = do
      diff a (<) b
      checkResults (b:rest)
