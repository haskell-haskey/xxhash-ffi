{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.QuickCheck

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BS
import Data.Digest.XXHash.FFI (XXH3 (..), xxh32, xxh64)
import Data.Digest.XXHash.FFI.C
import Data.Semigroup ((<>))
import Data.Word (Word32, Word64)
import Prelude hiding ((<>))

import Data.Bits
import Data.Hashable
import Foreign.C

instance Arbitrary BL.ByteString where
  arbitrary = BL.pack <$> arbitrary
  shrink bs = BL.pack <$> shrink (BL.unpack bs)

instance Arbitrary BS.ByteString where
  arbitrary = BS.pack <$> arbitrary
  shrink bs = BS.pack <$> shrink (BS.unpack bs)

main :: IO ()
main =
  defaultMain $
    testGroup "All" $
      [ testGroup
          "xxh32 strict"
          [ testProperty "<empty>" $
              xxh32bs' "" === 0x02cc5d05
          , testProperty "Hello World" $
              xxh32bs' "Hello World" === 0xb1fd16ee
          , testProperty "xxhash is a hashing library" $
              xxh32bs' "xxhash is a hashing library" === 0x5e213914
          ]
      , testGroup
          "xxh64 strict"
          [ testProperty "<empty>" $
              xxh64bs' "" === 0xef46db3751d8e999
          , testProperty "Hello World" $
              xxh64bs' "Hello World" === 0x6334d20719245bc2
          , testProperty "xxhash is a hashing library" $
              xxh64bs' "xxhash is a hashing library" === 0xea6cd1701a857e7c
          ]
      , testGroup
          "xxh3 strict"
          [ testProperty "<empty>" $
              xxh3bs' ""
                === if finiteBitSize (0 :: Int) == 64 then -8622991962414631331 else -1052714159
          , testProperty "Hello World" $
              xxh3bs' "Hello World"
                === if finiteBitSize (0 :: Int) == 64 then 7304763729587342359 else 207324119
          , testProperty "xxhash is a hashing library" $
              xxh3bs' "xxhash is a hashing library"
                === if finiteBitSize (0 :: Int) == 64 then 2442613548865080779 else 419890613
          ]
      , testGroup
          "xxh32 lazy"
          [ testProperty "<empty>" $
              xxh32bs (BL.fromChunks [""]) === 0x02cc5d05
          , testProperty "Hello World" $
              xxh32bs (BL.fromChunks ["Hello ", "World"]) === 0xb1fd16ee
          , testProperty "xxhash is a hashing library" $
              xxh32bs (BL.fromChunks ["xxhash is ", "a hashing ", "library"]) === 0x5e213914
          ]
      , testGroup
          "xxh64 lazy"
          [ testProperty "<empty>" $
              xxh64bs (BL.fromChunks [""]) === 0xef46db3751d8e999
          , testProperty "Hello World" $
              xxh64bs (BL.fromChunks ["Hello ", "World"]) === 0x6334d20719245bc2
          , testProperty "xxhash is a hashing library" $
              xxh64bs (BL.fromChunks ["xxhash is ", "a hashing ", "library"]) === 0xea6cd1701a857e7c
          ]
      , testGroup
          "xxh3 lazy"
          [ testProperty "<empty>" $
              xxh3bs (BL.fromChunks [""])
                === if finiteBitSize (0 :: Int) == 64 then -8622991962414631331 else -1052714159
          , testProperty "Hello World" $
              xxh3bs (BL.fromChunks ["Hello ", "World"])
                === if finiteBitSize (0 :: Int) == 64 then 7304763729587342359 else 207324119
          , testProperty "xxhash is a hashing library" $
              xxh3bs (BL.fromChunks ["xxhash is ", "a hashing ", "library"])
                === if finiteBitSize (0 :: Int) == 64 then 2442613548865080779 else 419890613
          ]
      , testGroup
          "lazy and strict"
          [ testProperty "xxh32" $ \(bs :: BL.ByteString) ->
              xxh32 bs 0 === xxh32 (BL.toStrict bs) 0
          , testProperty "xxh64" $ \(bs :: BL.ByteString) ->
              xxh64 bs 0 === xxh64 (BL.toStrict bs) 0
          , testProperty "xxh3" $ \(bs :: BL.ByteString) ->
              hash (XXH3 bs) === hash (XXH3 (BL.toStrict bs))
          ]
      , testGroup
          "Streaming API (64 bit)"
          [ testProperty "checking streaming and non streaming equivalence" $ \(a, b, c, seed) -> do
              let hsh = xxh64 (a <> b <> c) seed
              monadicIO $ do
                (CULLong shash) <- run $ allocaXXH64State $ \state -> do
                  c_xxh64_reset state (CULLong seed)
                  xxh64Update state a
                  xxh64Update state b
                  xxh64Update state c
                  c_xxh64_digest state
                assert (shash == hsh)
          ]
      , testGroup
          "Streaming API (32 bit)"
          [ testProperty "checking streaming and non streaming equivalence" $ property streaming32Equivalence
          ]
      ]

streaming32Equivalence :: (ByteString, ByteString, ByteString, Word32) -> Property
streaming32Equivalence (a, b, c, seed) = monadicIO $ do
  let hsh = xxh32 (a <> b <> c) seed
  (CUInt shash) <- run $ allocaXXH32State $ \state -> do
    c_xxh32_reset state (CUInt seed)
    xxh32Update state a
    xxh32Update state b
    xxh32Update state c
    c_xxh32_digest state
  assert $ shash == hsh

xxh32bs' :: ByteString -> Word32
xxh32bs' = flip xxh32 0

xxh64bs' :: ByteString -> Word64
xxh64bs' = flip xxh64 0

xxh3bs' :: ByteString -> Int
xxh3bs' = hash . XXH3

xxh32bs :: BL.ByteString -> Word32
xxh32bs = flip xxh32 0

xxh64bs :: BL.ByteString -> Word64
xxh64bs = flip xxh64 0

xxh3bs :: BL.ByteString -> Int
xxh3bs = hash . XXH3

use :: BS.ByteString -> (CString -> CSize -> IO a) -> IO a
use bs k = BS.unsafeUseAsCStringLen bs $ \(ptr, len) -> k ptr (fromIntegral len)

xxh3Update_64bits :: XXH3State -> BS.ByteString -> IO ()
xxh3Update_64bits state bs = use bs (c_xxh3_64bits_update state)

xxh64Update :: XXH64State -> BS.ByteString -> IO ()
xxh64Update state bs = use bs (c_xxh64_update state)

xxh32Update :: XXH64State -> BS.ByteString -> IO ()
xxh32Update state bs = use bs (c_xxh32_update state)
