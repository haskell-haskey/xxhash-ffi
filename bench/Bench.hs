{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Main where

import Prelude hiding (words)

import qualified Data.ByteString as B
import qualified Data.Digest.Adler32 as Adler32
import qualified Data.Digest.CRC32 as CRC32
import qualified Data.Digest.XXHash.FFI as FFI
#ifdef MIN_VERSION_hashable
import qualified Data.Hashable as Hashable
#endif

import Test.Tasty.Bench

main :: IO ()
main = do
    -- apt-get install wamerican
    med <- B.readFile "/usr/share/dict/words"

    defaultMain
        [ bgroup "10B"
            [ bench "Data.Digest.XXHash.FFI.xxh32" $ nf (FFI.xxh32 bs10b) 0
            , bench "Data.Digest.XXHash.FFI.xxh64" $ nf (FFI.xxh64 bs10b) 0
            , bench "Data.Digest.Adler32" $ nf Adler32.adler32 bs10b
            , bench "Data.Digest.CRC32" $ nf CRC32.crc32 bs10b
#ifdef MIN_VERSION_hashable
            , bench "Data.Digest.XXHash.FFI.XXH3" $ nf Hashable.hash (FFI.XXH3 bs10b)
            , bench "Data.Hashable" $ nf Hashable.hash bs10b
#endif
            ]
        , bgroup "1kB"
            [ bench "Data.Digest.XXHash.FFI.xxh32" $ nf (FFI.xxh32 bs1k) 0
            , bench "Data.Digest.XXHash.FFI.xxh64" $ nf (FFI.xxh64 bs1k) 0
            , bench "Data.Digest.Adler32" $ nf Adler32.adler32 bs1k
            , bench "Data.Digest.CRC32" $ nf CRC32.crc32 bs1k
#ifdef MIN_VERSION_hashable
            , bench "Data.Digest.XXHash.FFI.XXH3" $ nf Hashable.hash (FFI.XXH3 bs1k)
            , bench "Data.Hashable" $ nf Hashable.hash bs1k
#endif
            ]
        , bgroup "4kB"
            [ bench "Data.Digest.XXHash.FFI.xxh32" $ nf (FFI.xxh32 bs4k) 0
            , bench "Data.Digest.XXHash.FFI.xxh64" $ nf (FFI.xxh64 bs4k) 0
            , bench "Data.Digest.Adler32" $ nf Adler32.adler32 bs4k
            , bench "Data.Digest.CRC32" $ nf CRC32.crc32 bs4k
#ifdef MIN_VERSION_hashable
            , bench "Data.Digest.XXHash.FFI.XXH3" $ nf Hashable.hash (FFI.XXH3 bs4k)
            , bench "Data.Hashable" $ nf Hashable.hash bs4k
#endif
            ]
        , bgroup "1MB"
            [ bench "Data.Digest.XXHash.FFI.xxh32" $ nf (FFI.xxh32 med) 0
            , bench "Data.Digest.XXHash.FFI.xxh64" $ nf (FFI.xxh64 med) 0
            , bench "Data.Digest.Adler32" $ nf Adler32.adler32 med
            , bench "Data.Digest.CRC32" $ nf CRC32.crc32 med
#ifdef MIN_VERSION_hashable
            , bench "Data.Digest.XXHash.FFI.XXH3" $ nf Hashable.hash (FFI.XXH3 med)
            , bench "Data.Hashable" $ nf Hashable.hash med
#endif
            ]
        ]

bs10b :: B.ByteString
bs10b = fst $ B.unfoldrN 10 (\x -> Just (x, x)) 0

bs1k :: B.ByteString
bs1k = fst $ B.unfoldrN 1024 (\x -> Just (x, x)) 0

bs4k :: B.ByteString
bs4k = fst $ B.unfoldrN 4096 (\x -> Just (x, x)) 0
