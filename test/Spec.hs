{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck

import Control.Applicative ((<$>))

import Data.ByteString (ByteString)
import Data.Word (Word32, Word64)
import Foreign.C.Types (CULLong(..), CUInt(..))
import Data.Monoid ((<>))
import Data.Digest.XXHash.FFI.C
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS

import Data.Digest.XXHash.FFI (xxh32, xxh64)

instance Arbitrary BL.ByteString where
    arbitrary = BL.pack <$> arbitrary
    shrink bs = BL.pack <$> shrink (BL.unpack bs)

instance Arbitrary BS.ByteString where
    arbitrary = BS.pack <$> arbitrary
    shrink bs = BS.pack <$> shrink (BS.unpack bs)

main :: IO ()
main = hspec suite

suite :: Spec
suite = do
    describe "xxh32 strict" $
        it "hashes known pairs" $ do
            xxh32bs' "" `shouldBe` 0x02cc5d05
            xxh32bs' "Hello World" `shouldBe` 0xb1fd16ee
            xxh32bs' "xxhash is a hashing library" `shouldBe` 0x5e213914

    describe "xxh64 strict" $
        it "hashes known pairs" $ do
            xxh64bs' "" `shouldBe` 0xef46db3751d8e999
            xxh64bs' "Hello World" `shouldBe` 0x6334d20719245bc2
            xxh64bs' "xxhash is a hashing library" `shouldBe` 0xea6cd1701a857e7c

    describe "xxh32 lazy" $
        it "hashes known pairs" $ do
            xxh32bs (BL.fromChunks [""]) `shouldBe` 0x02cc5d05
            xxh32bs (BL.fromChunks ["Hello ", "World"]) `shouldBe` 0xb1fd16ee
            xxh32bs (BL.fromChunks ["xxhash is ", "a hashing ", "library"]) `shouldBe` 0x5e213914

    describe "xxh64 lazy" $
        it "hashes known pairs" $ do
            xxh64bs (BL.fromChunks [""]) `shouldBe` 0xef46db3751d8e999
            xxh64bs (BL.fromChunks ["Hello ", "World"]) `shouldBe` 0x6334d20719245bc2
            xxh64bs (BL.fromChunks ["xxhash is ", "a hashing ", "library"]) `shouldBe` 0xea6cd1701a857e7c
    describe "lazy and strict" $
        prop "hashes lazy and strict" $ \(bs :: BL.ByteString) ->
            xxh64 bs 0 == xxh64 (BL.toStrict bs) 0
    describe "Streaming API (64 bit)" $
        prop "checking streaming and non streaming equivalence" $ \(a, b, c, seed) -> do
            let hash = xxh64 (a <> b <> c) seed
            monadicIO $ do
              (CULLong shash) <- run $ allocaXXH64State $ \state -> do
                                   c_xxh64_reset state (CULLong seed)
                                   xxh64Update state a
                                   xxh64Update state b
                                   xxh64Update state c
                                   c_xxh64_digest state
              assert (shash == hash)
    describe "Streaming API (32 bit)" $
        prop "checking streaming and non streaming equivalence" $ property streaming32Equivalence

streaming32Equivalence :: (ByteString, ByteString, ByteString, Word32) -> Property
streaming32Equivalence (a,b,c,seed) = monadicIO $ do
  let hash = xxh32 (a <> b <> c) seed
  (CUInt shash) <- run $ allocaXXH32State $ \state -> do
                                                    c_xxh32_reset state (CUInt seed)
                                                    xxh32Update state a
                                                    xxh32Update state b
                                                    xxh32Update state c
                                                    c_xxh32_digest state
  assert $ shash == hash



xxh32bs' :: ByteString -> Word32
xxh32bs' = flip xxh32 0

xxh64bs' :: ByteString -> Word64
xxh64bs' = flip xxh64 0

xxh32bs :: BL.ByteString -> Word32
xxh32bs = flip xxh32 0

xxh64bs :: BL.ByteString -> Word64
xxh64bs = flip xxh64 0
