{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Monad          (unless, void)
import           Control.Monad.IO.Class (MonadIO)
import           Data.List              (nub)
import           Data.Snowchecked
import           Data.WideWord.Word256
import           Data.Word
import           Gens
import           Hedgehog
import qualified Hedgehog.Gen           as Gen
import           Hedgehog.Main          (defaultMain)
import qualified Hedgehog.Range         as Range

import qualified Integer
import qualified Word32
import qualified Word64

main :: IO ()
main = do
		recheck (Size 4) (Seed 14462958355020818941 14776761114051845121) prop_generatesUniqueValues
		recheck (Size 8) (Seed 11763301661976410488 14055395789257366631) prop_generatesUniqueValues
		recheck (Size 14) (Seed 6771904241892528611 8532317410904456029) prop_generatesUniqueValues
		recheck (Size 14) (Seed 4790609826115340731 8375105224114527375) prop_generatesUniqueValues
		defaultMain
			[ checkParallel $$(discover)
			, Integer.tests
			, Word32.tests
			, Word64.tests
			]

prop_generatesUniqueValues :: Property
prop_generatesUniqueValues = property $ do
	lst <- forAll $ Gen.list (Range.linear 2 8192) (return ())
	cfg <- forAll genConfig
	unless ( uniqueFlakeCount cfg > toInteger (length lst) ) discard
	nodeId <- forAll genWord256
	flakeGen <- newSnowcheckedGen cfg nodeId
	resultLst <- mapM (\_ -> nextFlake flakeGen) lst
	resultLst === nub resultLst

prop_flakeCanBeNFed :: Property
prop_flakeCanBeNFed = property $ do
	flake <- forAllFlake
	void $ evalNF flake
