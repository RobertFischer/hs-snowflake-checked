{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad          (unless, void)
import           Control.Monad.IO.Class (MonadIO)
import           Data.List              (nub)
import           Data.Snowchecked
import           Data.Snowchecked.Encoding.Integral
import           Data.WideWord.Word256
import           Data.Word
import           Gens
import           Hedgehog
import qualified Hedgehog.Gen           as Gen
import           Hedgehog.Main          (defaultMain)
import qualified Hedgehog.Range         as Range

import qualified Integer
import qualified String
import qualified Text
import qualified Word32
import qualified Word64

main :: IO ()
main =
  defaultMain
    [ checkParallel $$(discover)
    , Integer.tests
    , Word32.tests
    , Word64.tests
    , Text.tests
    , String.tests
    ]

prop_confGenerationWorks :: Property
prop_confGenerationWorks = property $
  void (forAll genConfig)

uniqueGenerationProperty :: (SnowcheckedConfig -> SnowcheckedConfig) -> Property
uniqueGenerationProperty nudge = 
  property $ do
    cfg <- nudge <$> forAll genConfig
    flakeGen <- forAll genWord256 >>= newSnowcheckedGen cfg
    let flakeCount = min 4096 (fromIntegral $ uniqueFlakeCount cfg)
    lst <- forAll $ Gen.list (Range.linear 2 flakeCount) (pure ())
    resultLst <- mapM (const $ nextFlake flakeGen) lst
    resultLst === nub resultLst

prop_generatesUniqueValues :: Property
prop_generatesUniqueValues = uniqueGenerationProperty id

prop_flakeCanBeNFed :: Property
prop_flakeCanBeNFed = property $
  forAllFlake >>= void . evalNF

prop_generatesUniqueValuesWithZeroCheckBits :: Property
prop_generatesUniqueValuesWithZeroCheckBits =
  uniqueGenerationProperty (\cfg -> cfg { confCheckBits = 0 })

prop_generatesUniqueValuesWithZeroNodeIdBits :: Property
prop_generatesUniqueValuesWithZeroNodeIdBits = 
  uniqueGenerationProperty (\cfg -> cfg { confNodeBits = 0 })

prop_generatesUniqueValuesWithZeroCountBits :: Property
prop_generatesUniqueValuesWithZeroCountBits = 
  uniqueGenerationProperty (\cfg -> cfg { confCountBits = 0 })

prop_monotonicallyIncreasing :: Property
prop_monotonicallyIncreasing = property $ do
    cfg <- forAll genConfig
    let flakeCount = min 1024 (fromIntegral $ uniqueFlakeCount cfg)
    lst <- forAll $ Gen.list (Range.linear 2 flakeCount) (return ())
    nodeId <- forAll genWord256
    flakeGen <- newSnowcheckedGen cfg nodeId
    resultLst <- mapM (\_ -> nextFlake flakeGen) lst
    checkResults resultLst
  where
    checkResults [] = success
    checkResults [_] = success
    checkResults (a:b:rest) = do
      diff a (<) b
      checkResults (b:rest)
