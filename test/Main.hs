module Main where

import           Test.Hspec

import           Data.IDX
import           Data.IDX.Internal

import qualified Data.Vector.Unboxed as V

import System.Directory
import System.IO

dataList = [1,2,3,4]

testData = IDXInts IDXUnsignedByte dims values
  where
    dims = V.fromList [2,2]
    values = V.fromList dataList

testLabels = IDXLabels $ V.fromList [0,1]

spec :: Spec
spec = do
  describe "IDX dataset" $ do
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
