{-# LANGUAGE GADTs #-}
--------------------------------------------------------------------------------
-- |
-- Module:    Data.IDX.Internal
-- Copyright: Christof Schramm
-- License:   GPLv3
--
-- Maintainer:  Christof Schramm <christof.schramm@campus.lmu.de>
-- Stability:   Experimental
-- Portability: Shoud work with all major haskell implementations
--
-- An internal package, the API contained here should not be used
-- and is subject to possibly breaking changes. Use these functions
-- and types at your own risk.
--
-- The safe interface is in 'Data.IDX'
--------------------------------------------------------------------------------
module Data.IDX.Internal where

import           Control.Monad (replicateM)
import           Data.Binary

import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed ((!))

-- | A type to describe the content, according to IDX spec
data IDXContentType where
   IDXUnsignedByte :: IDXContentType
   IDXSignedByte   :: IDXContentType
   IDXShort        :: IDXContentType
   IDXInt          :: IDXContentType
   IDXFloat        :: IDXContentType
   IDXDouble       :: IDXContentType
   deriving Show


-- | Datatype for storing IDXData. Internally data is always stored either
-- as 'Int' or 'Double' unboxed vectors. However when binary serialization
-- is used, the data is serialized according to the 'IDXContentType'.
data IDXData = IDXInts    IDXContentType (V.Vector Int) (V.Vector Int   )
             | IDXDoubles IDXContentType (V.Vector Int) (V.Vector Double)
             deriving Show

newtype IDXLabels = IDXLabels (V.Vector Int)


-- | Return the what type the data is stored in
idxType :: IDXData -> IDXContentType
idxType (IDXInts    t _ _) = t
idxType (IDXDoubles t _ _) = t

-- | Return an unboxed Vector of Int dimensions
idxDimensions :: IDXData -> V.Vector Int
idxDimensions (IDXInts    _ ds _) = ds
idxDimensions (IDXDoubles _ ds _) = ds

-- | Return wether the data in this IDXData value is
-- stored as integral values
isIDXIntegral :: IDXData -> Bool
isIDXIntegral (IDXInts _ _ _) = True
isIDXIntegral (_            ) = False

-- | Return wether the data in this IDXData value is
-- stored as double values
isIDXReal :: IDXData -> Bool
isIDXReal (IDXDoubles _ _ _) = True
isIDXReal (_               ) = False

-- | Return contained ints, if no ints are contained,
-- convert content to ints by using 'round'. Data is stored like
-- in a C-array, i.e. the last index changes first.
idxIntContent :: IDXData -> V.Vector Int
idxIntContent (IDXInts    _ _ v) = v
idxIntContent (IDXDoubles _ _ v) =
  V.fromList $ [round $ (v ! i) | i <- [0.. ((V.length v)-1)]]

-- | Return contained doubles, if no doubles are contained
-- convert the content to double by using 'fromIntegral'. Data is stored like
-- in a C-array, i.e. the last index changes first.
idxDoubleContent :: IDXData -> V.Vector Double
idxDoubleContent (IDXDoubles _ _ v) = v
idxDoubleContent (IDXInts    _ _ v) =
  V.fromList $ [fromIntegral $ (v ! i) | i <- [0.. ((V.length v) - 1)]]

-- | Helper function to read a (possibly big) vector of binary
-- values as chunks. Strictly evaluates each chunk and then
-- concatenates the chunks, does not leak space.
readContent :: (V.Unbox a)
            => (Int -> Get (V.Vector a)) -- ^ To Get a chunk of size n
            -> Int                       -- ^ Chunk size
            -> Int                       -- ^ Expected input
            -> Get (V.Vector a) 
readContent readEntries chunkSize n =
  if n > chunkSize
  then do
    headChunk <- readEntries (n `mod` chunkSize)
    let nChunks = n `div` chunkSize
    chunkList <- replicateM nChunks (readContent readEntries chunkSize chunkSize)
    return $! V.concat $ headChunk:chunkList
  else do
    rest <- readEntries n
    return $! rest

