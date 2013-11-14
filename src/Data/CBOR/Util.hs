module Data.CBOR.Util (
  toInt, decodeType, packType, minorBits, majorBits
) where

import Data.Bits
import Data.Word

-- | Bit mask for type portion of header byte
majorBits :: Word8
majorBits = 224 -- 11100000

-- | Bit mask for extra data portion of header byte
minorBits :: Word8
minorBits = 31 -- 00011111

-- | Convenience function to coerce one kind of integral into another
--
-- >>> let x = 42 :: Word8
-- >>> toInt x :: Int
-- 42
toInt :: (Integral a, Num b) => a -> b
toInt = fromInteger . toInteger

-- | Parses a CBOR header byte to extract the type and extra data
--
-- >>> decodeType 214
-- (6,22)
decodeType :: Word8 -> (Word8, Word8)
decodeType x = (x `shiftR` 5, x .&. minorBits)

-- | Takes a tuple containing a type and extra data value and packs them into a CBOR header byte
--
-- >>> packType (6,22)
-- 214
packType :: (Word8, Word8) -> Word8
packType (mt, eb) = mt' .|. eb'
  where
    mt' = (mt `shiftL` 5) .&. majorBits
    eb' = eb .&. minorBits