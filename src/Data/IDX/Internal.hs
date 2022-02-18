{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
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

import           Control.Monad (replicateM, void)
import           Data.Binary
import           Data.Int

import qualified Data.Vector.Unboxed as V

data SomeIDXContentType where
  SomeIDXContentType :: IDXContentType a -> SomeIDXContentType

deriving instance Show SomeIDXContentType

instance Eq SomeIDXContentType where
  (SomeIDXContentType a) == (SomeIDXContentType b) =
    case (a, b) of
      (IDXUnsignedByte, IDXUnsignedByte) -> True
      (IDXSignedByte  , IDXSignedByte  ) -> True
      (IDXShort       , IDXShort       ) -> True
      (IDXInt         , IDXInt         ) -> True
      (IDXFloat       , IDXFloat       ) -> True
      (IDXDouble      , IDXDouble      ) -> True
      _ -> False

-- | A type to describe the content, according to IDX spec
data IDXContentType a where
   IDXUnsignedByte :: IDXContentType Word8
   IDXSignedByte   :: IDXContentType Int8
   IDXShort        :: IDXContentType Int16
   IDXInt          :: IDXContentType Int32
   IDXFloat        :: IDXContentType Float
   IDXDouble       :: IDXContentType Double

deriving instance Show (IDXContentType a)
deriving instance Eq (IDXContentType a)

instance Binary SomeIDXContentType where
    get = do
      w <- getWord8
      case w of
        0x08 -> return $ SomeIDXContentType IDXUnsignedByte
        0x09 -> return $ SomeIDXContentType IDXSignedByte
        0x0B -> return $ SomeIDXContentType IDXShort
        0x0C -> return $ SomeIDXContentType IDXInt
        0x0D -> return $ SomeIDXContentType IDXFloat
        0x0E -> return $ SomeIDXContentType IDXDouble
        _ -> fail $ "Unrecognized IDX content type: " ++ (show w)

    put (SomeIDXContentType IDXUnsignedByte) = put (0x08  :: Word8)
    put (SomeIDXContentType IDXSignedByte  ) = put (0x09  :: Word8)
    put (SomeIDXContentType IDXShort       ) = put (0x0B  :: Word8)
    put (SomeIDXContentType IDXInt         ) = put (0x0C  :: Word8)
    put (SomeIDXContentType IDXFloat       ) = put (0x0D  :: Word8)
    put (SomeIDXContentType IDXDouble      ) = put (0x0E  :: Word8)

-- | Datatype for storing IDXData. Internally data is always stored either
-- as 'Int' or 'Double' unboxed vectors. However when binary serialization
-- is used, the data is serialized according to the 'IDXContentType'.
data IDXData where
  IDXData :: (Binary a, V.Unbox a, Show a) => IDXContentType a -> V.Vector Int -> V.Vector a -> IDXData

deriving instance Show IDXData

instance Eq IDXData  where
  (IDXData ctypeA dimsA dataA) == (IDXData ctypeB dimsB dataB) =
    case (ctypeA, ctypeB) of
      (IDXUnsignedByte, IDXUnsignedByte) -> dimsA == dimsB && dataA == dataB
      (IDXSignedByte  , IDXSignedByte  ) -> dimsA == dimsB && dataA == dataB
      (IDXShort       , IDXShort       ) -> dimsA == dimsB && dataA == dataB
      (IDXInt         , IDXInt         ) -> dimsA == dimsB && dataA == dataB
      (IDXFloat       , IDXFloat       ) -> dimsA == dimsB && dataA == dataB
      (IDXDouble      , IDXDouble      ) -> dimsA == dimsB && dataA == dataB
      _ -> False

instance Binary IDXData where
    get = do
      -- Get header information (4 bytes total)
      void getWord8
      void getWord8 
      SomeIDXContentType idxTyp <- get
      nDimensions <- fromIntegral <$> getWord8

      -- Each dimension size is encoded as a 32 bit integer
      dimensionSizes <- replicateM nDimensions (fromIntegral <$> (get :: Get Int32) :: Get Int)
      let nEntries = product dimensionSizes
          dimV = V.fromList dimensionSizes

      -- Retrieve the data, depending on the type specified in the file
      -- Cast all integral types to Int and all decimal numbers tod double
      case idxTyp of
        t@IDXUnsignedByte -> buildResult nEntries t dimV get
        t@IDXSignedByte   -> buildResult nEntries t dimV get
        t@IDXShort        -> buildResult nEntries t dimV get
        t@IDXInt          -> buildResult nEntries t dimV get
        t@IDXFloat        -> buildResult nEntries t dimV get
        t@IDXDouble       -> buildResult nEntries t dimV get
        
    put d@(IDXData _ dimensions content) = do
      -- First four bytes are meta information
      putWord8 0
      putWord8 0
      -- Third byte is content type
      put $ idxType d
      
      -- Fourth byte is number of dimensions
      put $ (fromIntegral $ V.length dimensions :: Word8)

      -- Put size of each dimension as an Int32
      V.forM_ dimensions $ (\x -> put $! (fromIntegral x :: Int32))

      -- Put the individual values
      V.forM_ content put 
        
-- | A data type that holds 'Int' labels for a set of 'IDXData'
newtype IDXLabels = IDXLabels (V.Vector Int)

instance Binary IDXLabels where
  get = do
    _ <- get :: Get Int32
    nItems <- fromIntegral <$> (get :: Get Int32)
    let readEntries n = V.replicateM n $ fromIntegral <$> getWord8 >>= (return $!)
    v <- readContent readEntries 500 nItems
    return $ IDXLabels v

  put (IDXLabels v) = do
    put (0 :: Int32)
    let len = V.length v
    put (fromIntegral len :: Int32)
    V.forM_ v (\x -> put $! (fromIntegral x :: Word8))


-- | Return the what type the data is stored in
idxType :: IDXData -> SomeIDXContentType
idxType (IDXData    t _ _) = SomeIDXContentType t

-- | Return an unboxed Vector of Int dimensions
idxDimensions :: IDXData -> V.Vector Int
idxDimensions (IDXData _ ds _) = ds

-- | Return wether the data in this IDXData value is
-- stored as integral values
isIDXIntegral :: IDXData -> Bool
isIDXIntegral (IDXData IDXUnsignedByte _ _) = True
isIDXIntegral (IDXData IDXSignedByte   _ _) = True
isIDXIntegral (IDXData IDXShort        _ _) = True
isIDXIntegral (IDXData IDXInt          _ _) = True
isIDXIntegral (IDXData IDXFloat        _ _) = False
isIDXIntegral (IDXData IDXDouble       _ _) = False

-- | Return wether the data in this IDXData value is
-- stored as double values
isIDXReal :: IDXData -> Bool
isIDXReal = not . isIDXIntegral

-- | Return contained ints, if no ints are contained,
-- convert content to ints by using 'round'. Data is stored like
-- in a C-array, i.e. the last index changes first.
idxIntContent :: IDXData -> V.Vector Int
idxIntContent (IDXData IDXUnsignedByte _ v) = V.map fromIntegral v
idxIntContent (IDXData IDXSignedByte   _ v) = V.map fromIntegral v
idxIntContent (IDXData IDXShort        _ v) = V.map fromIntegral v
idxIntContent (IDXData IDXInt          _ v) = V.map fromIntegral v
idxIntContent (IDXData IDXDouble       _ v) = V.map round v
idxIntContent (IDXData IDXFloat        _ v) = V.map round v

-- | Return contained doubles, if no doubles are contained
-- convert the content to double by using 'fromIntegral'. Data is stored like
-- in a C-array, i.e. the last index changes first.
idxDoubleContent :: IDXData -> V.Vector Double
idxDoubleContent (IDXData t _ v) =
  case t of
    IDXUnsignedByte -> V.map fromIntegral v
    IDXSignedByte   -> V.map fromIntegral v
    IDXShort        -> V.map fromIntegral v
    IDXInt          -> V.map fromIntegral v
    IDXDouble       -> v
    IDXFloat        -> V.map realToFrac v

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

-- | Helper function for parsing integer data from the
-- IDX content. Returns a full IDX result.
buildResult :: (Binary a, V.Unbox a, Show a)
            => Int              -- ^ Expected number of entries
            -> IDXContentType a -- ^ Description of content
            -> V.Vector Int     -- ^ Dimension sizes
            -> Get a            -- ^ Monadic action to get content element
            -> Get IDXData
buildResult nEntries typ dimV getContent = do
  content <- readContent readEntries 500 nEntries
  return $ IDXData typ dimV content
  where
    readEntries n = V.replicateM n $ getContent >>= (return $!)

-- | Split data by the first dimension of the C-Array. This would e.g. split a
-- data-set of images into a list of data representing an individual image
partitionedData :: V.Unbox a => (IDXData -> V.Vector a) -> IDXData -> [V.Vector a]
partitionedData getContent idxData = do
  i <- [0 .. dim0 - 1]
  return $ (V.slice (i*entrySize) entrySize content)
 where
   dim0 = V.head $ idxDimensions idxData
   content = getContent idxData
   entrySize = (V.product $ idxDimensions idxData) `div` dim0
