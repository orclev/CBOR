{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Binary.IEEE754.HalfFloat where

import Data.CBOR
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Control.Applicative

putFloat16be :: HalfFloat -> Put
putFloat16be (HF x) = putWord16be x

getFloat16be :: Get HalfFloat
getFloat16be = HF <$> getWord16be