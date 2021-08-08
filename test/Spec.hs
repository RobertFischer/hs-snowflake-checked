{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Monad          (unless, void)
import           Control.Monad.IO.Class (MonadIO)
import           Data.List              (nub)
import           Data.Snowchecked
import           Data.WideWord.Word256
import           Data.Word
import           Hedgehog
import qualified Hedgehog.Gen           as Gen
import           Hedgehog.Main          (defaultMain)
import qualified Hedgehog.Range         as Range

main :: IO ()
main = do
		recheck (Size 4) (Seed 14462958355020818941 14776761114051845121) prop_generatesUniqueValues
		recheck (Size 8) (Seed 11763301661976410488 14055395789257366631) prop_generatesUniqueValues
		recheck (Size 14) (Seed 6771904241892528611 8532317410904456029) prop_generatesUniqueValues
		recheck (Size 14) (Seed 4790609826115340731 8375105224114527375) prop_generatesUniqueValues
		defaultMain [ checkParallel $$(discover) ]

genWord8 :: (MonadGen m) => m Word8
genWord8 = Gen.word8 $ Range.linear (minBound @Word8) (maxBound @Word8)

genWord256 :: (MonadGen m) => m Word256
genWord256 = Gen.integral $ Range.linear (minBound @Word256) (maxBound @Word256)

genConfig :: (MonadGen m) => m SnowcheckedConfig
genConfig = SnowcheckedConfig
		<$> Gen.integral (Range.linear 2 (maxBound @Word8))
		<*> Gen.integral (Range.linear 2 (maxBound @Word8))
		<*> Gen.integral (Range.linear 2 (maxBound @Word8))
		<*> Gen.integral (Range.linear 2 (maxBound @Word8))

forAllFlake :: (MonadIO m) => PropertyT m Flake
forAllFlake = forAll genConfig >>= forAllFlake'

forAllFlake' :: (MonadIO m) => SnowcheckedConfig -> PropertyT m Flake
forAllFlake' cfg = do
	nodeId <- forAll genWord256
	newSnowcheckedGen cfg nodeId >>= nextFlake

prop_generatesUniqueValues :: Property
prop_generatesUniqueValues = property $ do
	lst <- forAll $ Gen.list (Range.linear 2 8192) (return ())
	cfg <- forAll genConfig
	unless ( uniqueFlakeCount cfg > toInteger (length lst) ) discard
	nodeId <- forAll genWord256
	flakeGen <- newSnowcheckedGen cfg nodeId
	resultLst <- mapM (\_ -> nextFlake flakeGen) lst
	resultLst === nub resultLst

prop_roundTripIntegral :: Property
prop_roundTripIntegral = property $ do
	cfg <- forAll genConfig
	flake <- forAllFlake' cfg
	result <- parseFlake cfg $ flakeToIntegral @Integer flake
	flake === result

prop_flakeCanBeNFed :: Property
prop_flakeCanBeNFed = property $ do
	flake <- forAllFlake
	void $ evalNF flake
