--------------------------------------------------------------------------------
-- |
-- Module    : Data.IDX
-- Copyright : Christof Schramm
-- License   : GPL v 3
--
-- Maintainer  : Christof Schramm <blog@christof-schramm.net>
-- Stability   : Experimental
-- Portability : Should work in all common Haskell implementations
--
-- A package for reading and writing data in the IDX format.
-- This data format is used for machine-learning data sets like the
-- MNIST database of handwritten digits (<http://yann.lecun.com/exdb/mnist/>)
--------------------------------------------------------------------------------
module Data.IDX (
                -- * Data types
                  IDXData
                , IDXLabels
                , IDXContentType(..)

                -- * Accessing data
                , idxType
                , idxDimensions

                , isIDXReal
                , isIDXIntegral

                -- ** Raw data
                , idxDoubleContent
                , idxIntContent

                -- ** Partitioned data
                , partitionedDoubleData
                , partitionedIntData

                -- ** Labeled data
                , labeledIntData
                , labeledDoubleData

                -- * IO / Serialization

                -- ** IDXLabels
                  
                -- *** ByteString serialization
                , encodeIDXLabels
                , decodeIDXLabels

                -- *** FileIO
                , encodeIDXLabelsFile
                , decodeIDXLabelsFile
                  
                -- ** IDXData (e.g. images)
                  
                -- *** ByteString serialization
                , encodeIDX
                , decodeIDX

                -- *** File IO
                , encodeIDXFile
                , decodeIDXFile
                )where

-- For compatibility with versions of base < 4.8
import           Control.Applicative ((<$>))
import           Control.Monad

import           Data.Binary
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.IDX.Internal
import           Data.Int
import           Data.Traversable
import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed ((!))
import           Data.Word

-- | Partition a dataset and label each subpartition, return int values
labeledIntData :: IDXLabels -> IDXData -> Maybe [(Int, V.Vector Int)]
labeledIntData (IDXLabels v) dat =
  if V.length v == length partitionedData'
  then Just $ zip (V.toList v) partitionedData'
  else Nothing
  where
    partitionedData' = partitionedData idxIntContent dat

-- | Partition a dataset and label each subpartition, return double values
labeledDoubleData :: IDXLabels -> IDXData -> Maybe [(Int, V.Vector Double)]
labeledDoubleData (IDXLabels v) dat =
  if V.length v == length partitionedData
  then Just $ zip (V.toList v) partitionedData
  else Nothing
  where
    partitionedData = partitionedDoubleData dat

-- | Partition a dataset along the first dimension. If the data set contains
-- images this means splitting the dataset up into a list of images where each
-- 'Double' represents one pixel.
partitionedDoubleData :: IDXData -> [V.Vector Double]
partitionedDoubleData = partitionedData idxDoubleContent

-- | Partition a dataset along the first dimension. If the data set contains
-- images this means splitting the dataset up into a list of images where each
-- 'Int' represents one pixel.
partitionedIntData :: IDXData -> [V.Vector Int]
partitionedIntData = partitionedData idxIntContent

-- | Read labels from a file, return 'Nothing' if something doesn't work
decodeIDXLabelsFile :: FilePath -> IO (Maybe IDXLabels)
decodeIDXLabelsFile path = BL.readFile path >>= return . decodeIDXLabels

decodeIDXLabels :: BL.ByteString -> Maybe IDXLabels
decodeIDXLabels content = case decodeOrFail content of
                           Right (_,_,result) -> Just result
                           Left _             -> Nothing

-- | Read data from a file, return 'Nothing' if something doesn't work
encodeIDXLabelsFile :: IDXLabels -> FilePath -> IO ()
encodeIDXLabelsFile labs path = encodeFile path labs

encodeIDXLabels :: IDXLabels -> BL.ByteString
encodeIDXLabels = encode

decodeIDXFile :: FilePath -> IO (Maybe IDXData)
decodeIDXFile path = BL.readFile path >>= return . decodeIDX

decodeIDX :: BL.ByteString -> Maybe IDXData
decodeIDX content = case decodeOrFail content of
  Right (_,_,result) -> Just result
  Left _ -> Nothing

encodeIDXFile :: IDXData -> FilePath -> IO ()
encodeIDXFile idx path = encodeFile path idx

encodeIDX :: IDXData -> BL.ByteString
encodeIDX = encode
