module Main where
import Test.QuickCheck
import qualified Data.ByteString as BS
import Data.Word
import Data.CBOR
import Data.CBOR.Util
import Data.Binary.CBOR
import Test.Framework
import Test.QuickCheck.Property
import Test.Framework.Providers.QuickCheck2
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import Control.Applicative
import Data.Monoid


main :: IO ()
main = defaultMain tests
-- main = defaultMainWithOpts tests runnerOptions

runnerOptions = mempty {
    ropt_test_options = Just testOptions
  }

testOptions = mempty {
      topt_maximum_generated_tests = Just 5
    , topt_maximum_unsuitable_generated_tests = Just 1
    , topt_maximum_test_size = Just 1
    , topt_maximum_test_depth = Just 1
  }

tests = [testGroup "Group1" [
    testProperty "round_trip" prop_roundtrip
  ]]

prop_roundtrip :: CBOR -> Bool
prop_roundtrip x = x == runGet getCBOR (runPut $ putCBOR x)

--instance Arbitrary BS.ByteString where
--  arbitrary :: Gen BS.ByteString
--  arbitrary = 


instance Arbitrary CBOR where
  arbitrary = sized sizedCBOR

sizedCBOR :: Int -> Gen CBOR
sizedCBOR 0 = 
  oneof [
      CBOR_UInt <$> choose (0, toInt (maxBound :: Word64))
      , CBOR_SInt <$> (-) (-1) <$> choose (0, toInt (maxBound :: Word64))
      , CBOR_BS <$> arbitraryByteString
      , CBOR_TS <$> arbitraryTextString
      --, CBOR_HalfFloat <$> arbitrary -- TODO: how to decide if something needs half float precision?
      , CBOR_Float <$> arbitrary
      , CBOR_Double <$> arbitrary
      , return CBOR_NULL
      , return CBOR_Undefined
      , CBOR_Reserved <$> oneof [return 28,return 29,return 30]
      , CBOR_Unassigned <$> oneof (map return $ [0..19])
      , return CBOR_True
      , return CBOR_False
      , CBOR_Byte <$> arbitrary
  ]
sizedCBOR n = 
  oneof [
        CBOR_UInt <$> choose (0, toInt (maxBound :: Word64))
      , CBOR_SInt <$> (-) (-1) <$> choose (0, toInt (maxBound :: Word64))
      , CBOR_BS <$> arbitraryByteString
      , CBOR_TS <$> arbitraryTextString
      , CBOR_Array <$> listOf1 (sizedCBOR $ n `div` 2)
      , CBOR_Map <$> listOf1 ((,) <$> (sizedCBOR $ n `div` 2) `suchThat` allowedKeyTypes <*> (sizedCBOR $ n `div` 2))
      , CBOR_Tag <$> choose (0, toInt (maxBound :: Word64)) <*> (sizedCBOR $ n `div` 2)
      --, CBOR_HalfFloat <$> arbitrary
      , CBOR_Float <$> arbitrary
      , CBOR_Double <$> arbitrary
      , return CBOR_NULL
      , return CBOR_Undefined
      , CBOR_Reserved <$> oneof [return 28,return 29,return 30]
      , CBOR_Unassigned <$> oneof (map return $ [0..19])
      , return CBOR_True
      , return CBOR_False
      , CBOR_Byte <$> arbitrary
    ]

allowedKeyTypes :: CBOR -> Bool
allowedKeyTypes x = case x of
  (CBOR_UInt _) -> True
  (CBOR_SInt _) -> True
  (CBOR_BS _) -> True
  (CBOR_TS _) -> True
  (CBOR_Tag _ y) -> allowedKeyTypes y
  (CBOR_HalfFloat _) -> True
  (CBOR_Float _) -> True
  (CBOR_Double _) -> True
  (CBOR_Reserved _) -> True
  (CBOR_Unassigned _) -> True
  CBOR_True -> True
  CBOR_False -> True
  CBOR_NULL -> True
  (CBOR_Byte _) -> True
  _ -> False

arbitraryByteString :: Gen BS.ByteString
arbitraryByteString = BS.pack <$> listOf1 arbitrary

arbitraryTextString :: Gen BS.ByteString
arbitraryTextString = BS.pack <$> listOf1 (choose (0, 0x79))
  -- Technically it can be higher than this, but then it must be encoded as multiple bytes 