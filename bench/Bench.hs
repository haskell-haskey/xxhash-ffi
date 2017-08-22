{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Prelude hiding (words)

import qualified Data.ByteString as B
import qualified Data.Digest.Adler32 as Adler32
import qualified Data.Digest.CRC32 as CRC32
import qualified Data.Digest.XXHash as XXHash
import qualified Data.Digest.XXHash.FFI as FFI
import qualified Data.Hashable as Hashable

import Criterion.Main

main :: IO ()
main = do
    -- apt-get install wamerican
    med <- B.readFile "/usr/share/dict/words"

    defaultMain
        [ bgroup "1MB"
            [ bench "Data.Digest.XXHash.FFI (32) (c)" $ nf (FFI.xxh32 med) 0
            , bench "Data.Digest.XXHash.FFI (64) (c)" $ nf (FFI.xxh64 med) 0
            , bench "xxhash (c)" $ nf XXHash.c_xxHash' med
            , bench "Data.Digest.XXHash (haskell)" $ nf XXHash.xxHash' med
            , bench "Data.Digest.adler32 (c, zlib)" $ nf Adler32.adler32 med
            , bench "Data.Digest.crc32 (c, zlib)" $ nf CRC32.crc32 med
            , bench "Data.Hashable (c, FNV?)" $ nf Hashable.hash med
            ]
        , bgroup "1kB"
            [ bench "Data.Digest.XXHash.FFI (32) (c)" $ nf (FFI.xxh32 bs1k) 0
            , bench "Data.Digest.XXHash.FFI (64) (c)" $ nf (FFI.xxh64 bs1k) 0
            , bench "xxhash (c)" $ nf XXHash.c_xxHash' bs1k
            , bench "Data.Digest.XXHash (haskell)" $ nf XXHash.xxHash' bs1k
            , bench "Data.Digest.adler32 (c, zlib)" $ nf Adler32.adler32 bs1k
            , bench "Data.Digest.crc32 (c, zlib)" $ nf CRC32.crc32 bs1k
            , bench "Data.Hashable (c, FNV?)" $ nf Hashable.hash bs1k
            ]
        , bgroup "4kB"
            [ bench "Data.Digest.XXHash.FFI (32) (c)" $ nf (FFI.xxh32 bs4k) 0
            , bench "Data.Digest.XXHash.FFI (64) (c)" $ nf (FFI.xxh64 bs4k) 0
            , bench "xxhash (c)" $ nf XXHash.c_xxHash' bs4k
            , bench "Data.Digest.XXHash (haskell)" $ nf XXHash.xxHash' bs4k
            , bench "Data.Digest.adler32 (c, zlib)" $ nf Adler32.adler32 bs4k
            , bench "Data.Digest.crc32 (c, zlib)" $ nf CRC32.crc32 bs4k
            , bench "Data.Hashable (c, FNV?)" $ nf Hashable.hash bs4k
            ]
        ]

bs1k :: B.ByteString
bs1k = fst $ B.unfoldrN 1024 (\x -> Just (x, x)) 0

bs4k :: B.ByteString
bs4k = fst $ B.unfoldrN 4096 (\x -> Just (x, x)) 0
