{-# LANGUAGE BangPatterns #-}
module Data.CBOR where

import Prelude hiding (take)
import Data.Bits
import Data.Word
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Binary.IEEE754
import qualified Data.Binary.Bits.Get as B
import qualified Data.Binary.Bits.Put as B
import Data.List (unfoldr)
import Control.Applicative
import Control.Monad (replicateM)
import qualified Data.ByteString as BS

data CBOR =   CBOR_UInt Integer
            | CBOR_SInt Integer
            | CBOR_BS BS.ByteString
            | CBOR_TS BS.ByteString
            | CBOR_Array [CBOR]
            | CBOR_Map [(CBOR,CBOR)]
            | CBOR_Tag Integer CBOR
            | CBOR_HalfFloat Float
            | CBOR_Float Float
            | CBOR_Double Double
            | CBOR_NULL
            | CBOR_Undefined
            | CBOR_Reserved Int
            | CBOR_Unassigned Int
            | CBOR_True
            | CBOR_False
            | CBOR_Byte Word8
            | CBOR_Stop
            deriving (Show, Eq)

majorBits :: Word8
majorBits = 224 -- 11100000

minorBits :: Word8
minorBits = 31 -- 00011111

unroll :: Integer -> [Word8]
unroll = unfoldr step
  where
    step 0 = Nothing
    step i = Just (fromIntegral i, i `shiftR` 8)

roll :: [Word8] -> Integer
roll   = foldr unstep 0
  where
    unstep b a = a `shiftL` 8 .|. fromIntegral b

bitShiftOr :: Int -> Int -> Int
{-# INLINE bitShiftOr #-}
bitShiftOr a x = (a `shiftL` 8) .|. x

decodeType :: Word8 -> (Word8, Word8)
decodeType x = (x `shiftR` 5, x .&. minorBits)

packType :: (Word8, Word8) -> Word8
packType (mt, eb) = mt' .|. eb'
  where
    mt' = (mt `shiftL` 5) .&. majorBits
    eb' = eb .&. minorBits

getHeaderBlock :: Get (Word8, Word8)
getHeaderBlock = B.runBitGet . B.block $ (,) <$> B.word8 3 <*> B.word8 5

putHeaderBlock :: Word8 -> Word8 -> Put
putHeaderBlock a b = B.runBitPut (B.putWord8 3 a >> B.putWord8 5 b)

getHeader :: Integral a => Get (Word8, a)
getHeader = do
  (a, b) <- getHeaderBlock
  ((,) a) <$> getSize b

putHeader :: Integral a => Word8 -> a -> Put
putHeader a b | b >= 4294967296 || b <= -4294967297 = putHeaderBlock a 27 >> putWord64be (toInt $ neg b)
              | b >= 65536 || b <= -65537 = putHeaderBlock a 26 >> putWord32be (toInt $ neg b)
              | b >= 256 || b <= -257 = putHeaderBlock a 25 >> putWord16be (toInt $ neg b)
              | b >= 24 || b <= -25 = putHeaderBlock a 24 >> putWord8 (toInt $ neg b)
              | b < 24 || b > -25 = putHeaderBlock a (toInt $ neg b)
  where
    neg x = if x < 0 then (x + 1) * (-1) else x 

getSize :: Integral a => Word8 -> Get a
getSize 31 = return $ -1
getSize 30 = fail "Additional information of 30 undefined for this type."
getSize 29 = fail "Additional information of 29 undefined for this type."
getSize 28 = fail "Additional information of 28 undefined for this type."
getSize 27 = toInt <$> getWord64be
getSize 26 = toInt <$> getWord32be
getSize 25 = toInt <$> getWord16be
getSize 24 = toInt <$> getWord8
getSize x = return $ toInt x

toInt :: (Integral a, Num b) => a -> b
toInt = fromInteger . toInteger

getCBOR :: Get CBOR
getCBOR = do
  (x, _) <- lookAhead getHeaderBlock
  case x of
    0 -> getUnsignedInt
    1 -> getNegativeInt
    2 -> getBS
    3 -> getTextString
    4 -> getArray
    5 -> getMap
    6 -> getTag
    7 -> getOther
    _ -> fail "Unknown CBOR type"

putCBOR :: CBOR -> Put
putCBOR (CBOR_UInt x) = putHeader 0 x
putCBOR (CBOR_SInt x) = putHeader 1 x
putCBOR (CBOR_BS x) = putHeader 2 (BS.length x) >> putByteString x
putCBOR (CBOR_TS x) = putHeader 3 (BS.length x) >> putByteString x
putCBOR (CBOR_Array x) = putHeader 4 (length x) >> mapM_ putCBOR x
putCBOR (CBOR_Map x) = putHeader 5 (length x) >> mapM_ putPair x
putCBOR (CBOR_Tag x y) = putHeader 6 x >> putCBOR y 
putCBOR CBOR_False = putHeaderBlock 7 20
putCBOR CBOR_True = putHeaderBlock 7 21
putCBOR CBOR_NULL = putHeaderBlock 7 22
putCBOR CBOR_Undefined = putHeaderBlock 7 23
putCBOR (CBOR_Byte x) = putHeaderBlock 7 24 >> putWord8 x
putCBOR (CBOR_HalfFloat x) = putHeaderBlock 7 25 >> putFloat32be x -- TODO: Make putFloat16be
putCBOR (CBOR_Float x) = putHeaderBlock 7 26 >> putFloat32be x
putCBOR (CBOR_Double x) = putHeaderBlock 7 27 >> putFloat64be x 
putCBOR (CBOR_Reserved 28) = putHeaderBlock 7 28
putCBOR (CBOR_Reserved 29) = putHeaderBlock 7 29
putCBOR (CBOR_Reserved 30) = putHeaderBlock 7 30
putCBOR (CBOR_Reserved _) = fail "Invalid reserved value"
putCBOR CBOR_Stop = putHeaderBlock 7 31
putCBOR (CBOR_Unassigned x) = putHeaderBlock 7 (toInt x)

putPair :: (CBOR, CBOR) -> Put
putPair (a,b) = putCBOR a >> putCBOR b

getUnsignedInt :: Get CBOR
getUnsignedInt = do
  CBOR_UInt . snd <$> getHeader

getNegativeInt :: Get CBOR
getNegativeInt = CBOR_SInt . neg . snd <$> getHeader
  where 
    neg a = -1 - a

getBS :: Get CBOR
getBS = do
  (_, x) <- getHeaderBlock
  case x of
    31 -> CBOR_BS <$> chunk
    _ -> CBOR_BS <$> (getSize x >>= getByteString)
  where
    chunk = do
      h <- getHeaderBlock
      case h of
        (7,31) -> return BS.empty
        (2, x) -> BS.append <$> (getSize x >>= getByteString) <*> chunk
        _ -> fail "Illegal chunk type found in indefinite bytestring"


getTextString :: Get CBOR
getTextString = do
  (_, x) <- getHeaderBlock
  case x of
    31 -> CBOR_TS <$> chunk
    _ -> CBOR_TS <$> (getSize x >>= getByteString)
  where
    chunk = do
      h <- getHeaderBlock
      case h of
        (7,31) -> return BS.empty
        (3, x) -> BS.append <$> (getSize x >>= getByteString) <*> chunk
        _ -> fail "Illegal chunk type found in indefinite textstring"

getArray :: Get CBOR
getArray = do
  (_, x) <- getHeaderBlock
  case x of
    31 -> CBOR_Array <$> chunk
    _ -> CBOR_Array <$> (getSize x >>= (flip replicateM) getCBOR)
  where
    chunk = do
      h <- getHeaderBlock
      case h of
        (7,31) -> return []
        _ -> (:) <$> getCBOR <*> chunk

getMap :: Get CBOR
getMap = do
  (_, x) <- getHeaderBlock
  case x of
    31 -> CBOR_Map <$> chunk
    _ -> CBOR_Map <$> (getSize x >>= (flip replicateM) getPair)
  where
    chunk = do
      h <- getHeaderBlock
      case h of
        (7,31) -> return []
        _ -> (:) <$> getPair <*> chunk

getPair :: Get (CBOR,CBOR)
getPair = (,) <$> getCBOR <*> getCBOR

getTag :: Get CBOR
getTag = do
  (_, x) <- getHeaderBlock
  CBOR_Tag <$> getSize x <*> getCBOR

getOther :: Get CBOR
getOther = do
  (_, x) <- getHeaderBlock
  case x of
    20 -> return CBOR_False
    21 -> return CBOR_True
    22 -> return CBOR_NULL
    23 -> return CBOR_Undefined
    24 -> CBOR_Byte <$> getWord8
    25 -> CBOR_HalfFloat <$> getFloat16be
    26 -> CBOR_Float <$> getFloat32be
    27 -> CBOR_Double <$> getFloat64be
    28 -> return $ CBOR_Reserved 28
    29 -> return $ CBOR_Reserved 29
    30 -> return $ CBOR_Reserved 30
    31 -> return CBOR_Stop
    x' -> return $ CBOR_Unassigned (toInt x')