{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving #-}
-- | Provides functions to serialize CBOR encoded values to/from ByteStrings.
--
--   Warning, this package is very alpha quality right now. I've attempted to
--   implement a useful subset of the CBOR standard, but the interface is still
--   being refined and subject to change. Do not use this for anything important!
module Data.CBOR ( CBOR(..)
                 , HalfFloat(..)
                 ) where

import Data.Word
import qualified Data.ByteString as BS

-- | Data type for CBOR values
data CBOR =   CBOR_UInt Integer
            | CBOR_SInt Integer
            | CBOR_BS BS.ByteString
            | CBOR_TS BS.ByteString -- ^ TextString. This SHOULD be a UTF8 string if the spec is being followed.
            | CBOR_Array [CBOR]
            | CBOR_Map [(CBOR,CBOR)]
            | CBOR_Tag Integer CBOR
            | CBOR_HalfFloat HalfFloat -- ^ N.B. Currently stored as an opaque value because I can't be bothered to implement my own half width float type.
            | CBOR_Float Float
            | CBOR_Double Double
            | CBOR_NULL
            | CBOR_Undefined -- ^ Undefined as per the CBOR specification, a value of this type will usually represent a serializatoin error.
            | CBOR_Reserved Int -- ^ Reserved, you probably shouldn't see any of these.
            | CBOR_Unassigned Int -- ^ Unassigned
            | CBOR_True
            | CBOR_False
            | CBOR_Byte Word8
            | CBOR_Stop -- ^ Stop marker for indefinite encoding scheme
            deriving (Show, Eq)

newtype HalfFloat = HF Word16 -- ^ Opaque value, actual implementation TODO
  deriving (Eq, Ord)

instance Show HalfFloat where
  show (HF _) = "HalfFloat" -- N.B. We don't really support rendering of half width floats