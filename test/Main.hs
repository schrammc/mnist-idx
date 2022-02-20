{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where


import           Data.IDX
import           Data.IDX.Internal

import           Control.Exception
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
  arbitrary = elements [ SomeIDXContentType IDXUnsignedByte
                       , SomeIDXContentType IDXSignedByte
                       , SomeIDXContentType IDXShort
                       , SomeIDXContentType IDXInt
                       , SomeIDXContentType IDXFloat
                       , SomeIDXContentType IDXDouble
                       ]

instance Arbitrary IDXData where
  arbitrary = do
    typ <- arbitrary :: Gen SomeIDXContentType
    numberOfDimensions <- choose (1, 5) :: Gen Int
    dimensionSizes <-
      V.fromList <$> replicateM numberOfDimensions (choose (1, 10)) :: Gen (V.Vector Int)
    case typ of
      SomeIDXContentType a ->
        IDXData a dimensionSizes . V.fromList
          <$> replicateM (product $ V.toList dimensionSizes) arbitrary

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
      -- Open a few temp files, immediately close the file handles
      -- (we don't need them)
      withTempFile "idx_test_img" $ \imgPath ->
        withTempFile "idx_test_lab" $ \labPath -> do

          -- Save our test data
          encodeIDXFile testData imgPath
          encodeIDXLabelsFile testLabels labPath

          -- Read it again
          Just idata <- decodeIDXFile imgPath
          Just ils@(IDXLabels ilabs) <- decodeIDXLabelsFile labPath

          -- Label the image data with the label data
          let Just lst = labeledIntData ils idata

          -- See if everything matches up
          V.length ilabs `shouldBe` 2
          length lst `shouldBe` 2
          fst (lst !! 1) `shouldBe` 1
          V.toList (idxIntContent idata) `shouldBe` fromIntegral <$> dataList

withTempFile :: FilePath -> (FilePath -> IO a) -> IO a
withTempFile fileName action = do
  tempdir <- getTemporaryDirectory
  bracket (createTempFile tempdir) removeFile action
  where
    createTempFile tempdir = do
      (filePath, fileHandle) <- openTempFile tempdir fileName
      hClose fileHandle
      pure filePath

main :: IO ()
main = hspec spec
