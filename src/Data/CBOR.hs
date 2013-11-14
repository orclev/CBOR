{-# LANGUAGE BangPatterns #-}
-- | Provides functions to serialize CBOR encoded values to/from ByteStrings
module Data.CBOR ( CBOR(..)
                 ) where

import Data.Word
import qualified Data.ByteString as BS

-- | Data type for CBOR values
data CBOR =   CBOR_UInt Integer
            | CBOR_SInt Integer
            | CBOR_BS BS.ByteString
            | CBOR_TS BS.ByteString
            | CBOR_Array [CBOR]
            | CBOR_Map [(CBOR,CBOR)]
            | CBOR_Tag Integer CBOR
            | CBOR_HalfFloat Float -- TODO: This should be a half float type
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

-- TODO: define show instance for CBOR