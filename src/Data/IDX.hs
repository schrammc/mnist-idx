{-# LANGUAGE GADTs #-}

--------------------------------------------------------------------------------
-- |
-- Module    : Data.IDX
-- Copyright : Christof Schramm
-- License   : GPL v 3
--
-- Maintainer : Christof Schramm <christof.schramm@campus.lmu.de>
-- Stability : Experimental
-- Portability : Should work in all common Haskell implementations
--
-- A package for reading and writing data in the IDX format.
-- This data format is used for machine-learning data sets like the
-- MINST database of handwritten digits (<http://yann.lecun.com/exdb/mnist/>)
--------------------------------------------------------------------------------
module Data.IDX (
                -- * Data types
                  IDXData
                , IDXContentType(..)

                -- * Accessing data
                , idxType
                , idxDimensions

                , isIDXReal
                , isIDXIntegral

                , idxDoubleContent
                , idxIntContent

                -- * IO / Serialization

                -- ** ByteString serialization
                , encodeIDX
                , decodeIDX

                -- ** File IO
                , encodeIDXFile
                , decodeIDXFile
                )where

import           Control.Monad
import           Data.Binary
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.Int
import           Data.Traversable
import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed ((!))
import           Data.Word

import Debug.Trace

-- | A type to describe the content, according to IDX spec
data IDXContentType where
   IDXUnsignedByte :: IDXContentType
   IDXSignedByte   :: IDXContentType
   IDXShort        :: IDXContentType
   IDXInt          :: IDXContentType
   IDXFloat        :: IDXContentType
   IDXDouble       :: IDXContentType
   deriving Show

instance Binary IDXContentType where
    get = do
      w <- getWord8
      case w of
        0x08 -> return IDXUnsignedByte
        0x09 -> return IDXSignedByte
        0x0B -> return IDXShort
        0x0C -> return IDXInt
        0x0D -> return IDXFloat
        0x0E -> return IDXDouble
        _ -> fail $ "Unrecognized IDX content type: " ++ (show w)
        
    put IDXUnsignedByte = putWord8 0x08
    put IDXSignedByte   = putWord8 0x09
    put IDXShort        = putWord8 0x0B
    put IDXInt          = putWord8 0x0C
    put IDXFloat        = putWord8 0x0D
    put IDXDouble       = putWord8 0x0E

-- | Datatype for storing IDXData. Internally data is always stored either
-- as 'Int' or 'Double' unboxed vectors. However when binary serialization
-- is used, the data is serialized according to the 'IDXContentType'.
data IDXData = IDXInts    IDXContentType (V.Vector Int) (V.Vector Int   )
             | IDXDoubles IDXContentType (V.Vector Int) (V.Vector Double)
             deriving Show

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
-- convert content to ints by using 'round'
idxIntContent :: IDXData -> V.Vector Int
idxIntContent (IDXInts    _ _ v) = v
idxIntContent (IDXDoubles _ _ v) =
  V.fromList $ [round $ (v ! i) | i <- [0.. V.length v]]

-- | Return contained doubles, if no doubles are contained
-- convert the content to double by using 'fromIntegral'
idxDoubleContent :: IDXData -> V.Vector Double
idxDoubleContent (IDXDoubles _ _ v) = v
idxDoubleContent (IDXInts    _ _ v) =
  V.fromList $ [fromIntegral $ (v ! i) | i <- [0.. V.length v]]

instance Binary IDXData where
    get = do
      -- Get header information (4 bytes total)
      getWord8
      getWord8 
      idxType <- get :: Get IDXContentType
      nDimensions <- fromIntegral <$> getWord8

      -- Each dimension size is encoded as a 32 bit integer
      dimensionSizes <- replicateM nDimensions (fromIntegral <$> getInt32)      
      let nEntries = fromIntegral $ product dimensionSizes
          dimV = V.fromList dimensionSizes

      -- Retrieve the data, depending on the type specified in the file
      -- Cast all integral types to Int and all decimal numbers tod double
      case idxType of
        t@IDXUnsignedByte -> buildIntResult nEntries t dimV getWord8
        t@IDXSignedByte   -> buildIntResult nEntries t dimV getInt8
        t@IDXShort        -> buildIntResult nEntries t dimV getInt16
        t@IDXInt          -> buildIntResult nEntries t dimV getInt32

        t@IDXFloat        -> buildDoubleResult nEntries t dimV getFloat
        t@IDXDouble       -> buildDoubleResult nEntries t dimV getDouble
        
    put d = do
      -- First four bytes are meta information
      putWord8 0
      putWord8 0
      -- Third byte is content type
      put $ idxType d
      
      -- Fourth byte is number of dimensions
      let dimensions = idxDimensions d
      put $ (fromIntegral $ V.length dimensions :: Word8)

      -- Put size of each dimension as an Int32
      V.forM_ dimensions $ (\x -> put $! (fromIntegral x :: Int32))

      -- Put the individual values
      case d of
        IDXDoubles t _ content -> V.forM_ content $ putReal     t
        IDXInts    t _ content -> V.forM_ content $ putIntegral t

-- | Helper function for parsing integer data from the
-- IDX content. Returns a full IDX result.
buildIntResult :: Integral a
                  => Int                -- ^ Expected number of entries
                      -> IDXContentType -- ^ Description of content
                      -> V.Vector Int   -- ^ Dimension sizes
                      -> Get a          -- ^ Monadic action to get content element
                      -> Get IDXData
buildIntResult nEntries typ dimV getContent = do
  content <- readContent readEntries 500 nEntries
  return $ IDXInts typ dimV content
  where
    readEntries n = V.replicateM n $ fromIntegral <$> getContent >>= (return $!)

-- | Helper function for parsing real number data from
-- the IDX content.
buildDoubleResult :: Real a
                  => Int                -- ^ Expected number of entries
                      -> IDXContentType -- ^ Description of content
                      -> V.Vector Int   -- ^ Dimension sizes
                      -> Get a          -- ^ Monadic action to get content element
                      -> Get IDXData
buildDoubleResult nEntries typ dimV getContent = do
  content <- readContent readEntries 500 nEntries
  return $ IDXDoubles typ dimV content
  where
    readEntries n = V.replicateM n $ realToFrac <$> getContent >>= (return $!)

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

-- | Put values that are saved as Int
putIntegral :: IDXContentType -> Int -> Put
putIntegral IDXUnsignedByte n = put $! (fromIntegral n :: Word8)
putIntegral IDXSignedByte   n = put $! (fromIntegral n :: Int8 )
putIntegral IDXShort        n = put $! (fromIntegral n :: Int16)
putIntegral IDXInt          n = put $! (fromIntegral n :: Int32)
putIntegral t               _ = error $ "IDX.putIntegral " ++ show t 

-- | Put real values that are saved as Double
putReal :: IDXContentType -> Double -> Put
putReal IDXDouble n = put n
putReal IDXFloat  n = put $! (realToFrac n :: Float )

-- Haskell's Data.Binary uses big-endian format
getInt8 :: Get Int8
getInt8 = get

getInt16 :: Get Int16
getInt16 = get

getInt32 :: Get Int32
getInt32 = get

getFloat :: Get Float
getFloat = get

getDouble :: Get Double
getDouble = get

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
