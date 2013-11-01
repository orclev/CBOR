{-# LANGUAGE BangPatterns #-}
import Prelude hiding (take)
import Data.Bits
import Data.Word
import Data.Binary
import Data.Binary.Get
import Data.Binary.IEEE754
import Data.List (unfoldr)
import Control.Applicative
import Control.Monad (replicateM)
import qualified Data.ByteString as BS

data CBOR =   CBOR_UInt Int
            | CBOR_SInt Int
            | CBOR_BS BS.ByteString
            | CBOR_TS BS.ByteString
            | CBOR_Array [CBOR]
            | CBOR_Map [(CBOR,CBOR)]
            | CBOR_Tag Int CBOR
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
            deriving (Show)

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

getCBOR :: Get CBOR
getCBOR = do
  x <- getWord8
  case decodeType x of
    (0, y) -> getUnsignedInt y 
    (1, y) -> getNegativeInt y
    (2, y) -> getBS y
    (3, y) -> getTextString y
    (4, y) -> getArray y
    (5, y) -> getMap y
    (6, y) -> getTag y
    (7, y) -> getOther y

getSize :: Word8 -> Get Int
getSize 31 = fail "Indefinite length not supported on this type."
getSize 30 = fail "Additional information of 30 undefined for this type."
getSize 29 = fail "Additional information of 29 undefined for this type."
getSize 28 = fail "Additional information of 28 undefined for this type."
getSize 27 = toInt <$> getWord64be
getSize 26 = toInt <$> getWord32be
getSize 25 = toInt <$> getWord16be
getSize x = return $ toInt x

toInt :: (Integral a, Num b) => a -> b
toInt = fromInteger . toInteger

getUnsignedInt :: Word8 -> Get CBOR
getUnsignedInt x = CBOR_UInt <$> getSize x

getNegativeInt :: Word8 -> Get CBOR
getNegativeInt x = CBOR_SInt . neg <$> getSize x
  where
    neg a = -1 - a

getBS :: Word8 -> Get CBOR
getBS 31 = CBOR_BS <$> chunk
  where
    chunk = do
      x <- lookAhead getWord8
      case decodeType x of
        (7,31) -> getWord8 >> return BS.empty -- stop bit
        (2, y) -> BS.append <$> (getSize y >>= getByteString) <*> chunk
        _ -> fail "Illegal chunk sequence"
getBS x = CBOR_BS <$> (getSize x >>= getByteString)

getTextString :: Word8 -> Get CBOR
getTextString 31 = CBOR_TS <$> chunk
  where
    chunk = do
      x <- lookAhead getWord8
      case decodeType x of
        (7,31) -> getWord8 >> return BS.empty
        (2, y) -> BS.append <$> (getSize y >>= getByteString) <*> chunk
        _ -> fail "Illegal chunk sequence"
getTextString x = CBOR_TS <$> (getSize x >>= getByteString)

getArray :: Word8 -> Get CBOR
getArray 31 = CBOR_Array <$> chunk
  where
    chunk = do
      x <- lookAhead getWord8
      case decodeType x of
        (7,31) -> getWord8 >> return []
        _ -> (:) <$> getCBOR <*> chunk
getArray x = do
  y <- getSize x
  CBOR_Array <$> replicateM y getCBOR

getMap :: Word8 -> Get CBOR
getMap 31 = CBOR_Map <$> chunk
  where
    chunk = do
      x <- lookAhead getWord8
      case decodeType x of
        (7,31) -> getWord8 >> return []
        _ -> (:) <$> getPair <*> chunk
getMap x = do
  y <- getSize x
  CBOR_Map <$> replicateM y getPair

getPair :: Get (CBOR,CBOR)
getPair = (,) <$> getCBOR <*> getCBOR

getTag :: Word8 -> Get CBOR
getTag x = do
  y <- getSize x
  CBOR_Tag y <$> getCBOR

getOther :: Word8 -> Get CBOR
getOther x = do
  case x of
    20 -> return CBOR_False
    21 -> return CBOR_True
    22 -> return CBOR_NULL
    23 -> return CBOR_Undefined
    24 -> CBOR_Byte <$> getWord8
    25 -> CBOR_Float <$> getFloat16be
    26 -> CBOR_Float <$> getFloat32be
    27 -> CBOR_Double <$> getFloat64be
    28 -> return $ CBOR_Reserved 28
    29 -> return $ CBOR_Reserved 29
    30 -> return $ CBOR_Reserved 30
    31 -> return CBOR_Stop
    x' -> return $ CBOR_Unassigned (fromInteger $ toInteger x')
