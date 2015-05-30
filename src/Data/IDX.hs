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
                , IDXLabels
                , IDXContentType(..)

                -- * Accessing data
                , idxType
                , idxDimensions

                , isIDXReal
                , isIDXIntegral

                -- * Raw data
                , idxDoubleContent
                , idxIntContent

                -- * Labeled data
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

import Debug.Trace

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

instance Binary IDXLabels where
  get = do
    getInt32
    nItems <- fromIntegral <$> getInt32
    let readEntries n = V.replicateM n $ fromIntegral <$> getWord8 >>= (return $!)
    v <- readContent readEntries 500 nItems
    return $ IDXLabels v

  put (IDXLabels v) = do
    put (0 :: Int32)
    let len = V.length v
    put (fromIntegral len :: Int32)
    V.forM_ v (\x -> put $! (fromIntegral x :: Word8))

-- | Partition a dataset and label each subpartition, return int values
labeledIntData:: IDXLabels -> IDXData -> Maybe [(Int, V.Vector Int)]
labeledIntData (IDXLabels v) dat =
  if (V.length v) == dim0
  then Just $ do
    i <- [0 .. dim0 - 1]
    let lab = v ! i
    return $ (lab,V.slice (i*entrySize) entrySize content)
  else Nothing
  where
    dim0 = (idxDimensions dat) ! 0
    content = idxIntContent dat
    entrySize = (V.product $ idxDimensions dat) `div` dim0

-- | Partition a dataset and label each subpartition, return double values
labeledDoubleData:: IDXLabels -> IDXData -> Maybe [(Int, V.Vector Double)]
labeledDoubleData (IDXLabels v) dat =
  if (V.length v) == dim0
  then Just $ do
    i <- [0 .. dim0 - 1]
    let lab = v ! i
    return $ (lab,V.slice (i*entrySize) entrySize content)
  else Nothing
  where
    dim0 = (idxDimensions dat) ! 0
    content = idxDoubleContent dat
    entrySize = (V.product $ idxDimensions dat) `div` dim0

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
