import Test.DocTest
main = doctest ["-isrc", "src/Data/CBOR.hs", "src/Data/CBOR/Util.hs", "src/Data/Binary/CBOR.hs"]