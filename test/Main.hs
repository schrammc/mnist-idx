{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where


import           Data.IDX
import           Data.IDX.Internal

import           Control.Monad
import qualified Data.Binary as Binary
import qualified Data.Vector.Unboxed as V
import           Data.Word
import           System.Directory
import           System.IO
import           Test.Hspec
import           Test.QuickCheck


dataList :: [Word8]
dataList = [1,2,3,4]

testData :: IDXData
testData = IDXData IDXUnsignedByte dims values
  where
    dims = V.fromList [2,2]
    values = V.fromList dataList

testLabels :: IDXLabels
testLabels = IDXLabels $ V.fromList [0,1]

instance Arbitrary SomeIDXContentType where
  arbitrary = elements [SomeIDXContentType IDXInt, SomeIDXContentType IDXDouble]

instance Arbitrary IDXData where
  arbitrary = do
    typ <- arbitrary :: Gen SomeIDXContentType
    numberOfDimensions <- choose (1, 5) :: Gen Int
    dimensionSizes <-
      V.fromList <$> replicateM numberOfDimensions (choose (1, 10)) :: Gen (V.Vector Int)
    case typ of
      SomeIDXContentType IDXInt ->
        IDXData IDXInt dimensionSizes . V.fromList
          <$> replicateM (product $ V.toList dimensionSizes) arbitrary
      SomeIDXContentType IDXDouble ->
        IDXData IDXDouble dimensionSizes . V.fromList
          <$> replicateM (product $ V.toList dimensionSizes) arbitrary
      _ -> error "This shouldn't happen"

spec :: Spec
spec = do
  describe "IDX dataset" $ do
    it "Binary serialization roundtrips" $
      property $ \(idxData :: IDXData) -> Binary.decode (Binary.encode idxData) == idxData

    it "Partitioning the dataset and concatenating partitions is identical (double)" $
      property $ \(idxData :: IDXData) -> mconcat (partitionedDoubleData idxData) == idxDoubleContent idxData

    it "Partitioning the dataset and concatenating partitions is identical (int)" $
      property $ \(idxData :: IDXData) -> mconcat (partitionedIntData idxData) == idxIntContent idxData

    it "should be created and deserialized correctly" $ do

      -- Get temporary directory
      tempdir <- getTemporaryDirectory

      -- Open a few temp files, immediately close the file handles
      -- (we don't need them)
      (imgPath,imgH) <- openTempFile tempdir "idx_test_img"
      hClose imgH

      (labPath,labH) <- openTempFile tempdir "idx_test_lab"
      hClose labH

      -- Save our test data
      encodeIDXFile testData imgPath
      encodeIDXLabelsFile testLabels labPath

      -- Read it again
      Just idata <- decodeIDXFile imgPath
      Just ils@(IDXLabels ilabs) <- decodeIDXLabelsFile labPath

      -- Remove the temp files
      removeFile imgPath
      removeFile labPath

      -- Label the image data with the label data
      let Just lst = labeledIntData ils idata

      -- See if everything matches up
      V.length ilabs `shouldBe` 2
      length lst `shouldBe` 2
      (fst $ head $ tail lst) `shouldBe` 1
--      (V.toList $ idxIntContent idata) `shouldBe` dataList

main :: IO ()
main = hspec spec
