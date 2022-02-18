{-# LANGUAGE ScopedTypeVariables #-}
module Main where


import           Data.IDX
import           Data.IDX.Internal

import           Control.Monad
import qualified Data.Vector.Unboxed as V
import           System.Directory
import           System.IO
import           Test.Hspec
import           Test.QuickCheck
import qualified Data.Binary as Binary


dataList :: [Int]
dataList = [1,2,3,4]

testData :: IDXData
testData = IDXInts IDXUnsignedByte dims values
  where
    dims = V.fromList [2,2]
    values = V.fromList dataList

testLabels :: IDXLabels
testLabels = IDXLabels $ V.fromList [0,1]

instance Arbitrary IDXContentType where
  arbitrary = elements [IDXInt, IDXDouble]

instance Arbitrary IDXData where
  arbitrary = do
    typ <- arbitrary :: Gen IDXContentType
    numberOfDimensions <- choose (1, 5) :: Gen Int
    dimensionSizes <- V.fromList <$> replicateM numberOfDimensions (choose (1, 10))
    case typ of
      IDXInt ->
        IDXInts typ dimensionSizes . V.fromList
          <$> replicateM (product $ V.toList dimensionSizes) arbitrary
      IDXDouble ->
        IDXDoubles typ dimensionSizes . V.fromList
          <$> replicateM (product $ V.toList dimensionSizes) arbitrary
      _ -> error "This shouldn't happen"

spec :: Spec
spec = do
  describe "IDX dataset" $ do
    it "Binary serialization roundtrips" $
      property $ \(idxData :: IDXData) -> Binary.decode (Binary.encode idxData) == idxData

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
      (V.toList $ idxIntContent idata) `shouldBe` dataList

main :: IO ()
main = hspec spec
