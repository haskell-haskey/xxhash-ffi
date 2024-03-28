{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- |
-- Module:      Data.Digest.XXHash.FFI
-- Copyright:   (c) 2017 Henri Verroken
-- Licence:     BSD3
-- Maintainer:  Henri Verroken <henriverroken@gmail.com
-- Stability:   stable
-- Portability: GHC
--
-- This module provides bindings to the xxHash64 and the xxHash32 algorithm.
--
-- The C implementation used is directly taken from <https://github.com/Cyan4973/xxHash>.
module Data.Digest.XXHash.FFI (
  -- * XXH3 interface
  XXH3 (..),

  -- * Deprecated interface
  XXHash (..),
) where

import Data.Digest.XXHash.FFI.C

import qualified Data.Array.Byte as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Hashable
import Data.Word (Word32, Word64)
import Foreign.C
import GHC.Exts (Int (..), Ptr (..), byteArrayContents#, isByteArrayPinned#, isTrue#, sizeofByteArray#)
import System.IO.Unsafe (unsafePerformIO)

{-# INLINE useBS #-}
useBS :: BS.ByteString -> (CString -> CSize -> IO a) -> IO a
useBS bs k = unsafeUseAsCStringLen bs $ \(ptr, len) -> k ptr (fromIntegral len)

{-# INLINE useBA #-}
useBA :: A.ByteArray -> (CString -> CSize -> IO a) -> IO a
useBA (A.ByteArray ba#) k = k (Ptr (byteArrayContents# ba#)) (fromIntegral (I# (sizeofByteArray# ba#)))

-- | Class for hashable data types.
--
-- Not that all standard instances are specialized using the @SPECIALIZE@
-- pragma.
class XXHash t where
  -- | Calculate the 32-bit xxHash using a given seed.
  xxh32
    :: t
    -- ^ Data to hash
    -> Word32
    -- ^ Seed
    -> Word32
    -- ^ Resulting hash

  -- | Calculate the 64-bit xxHash using a given seed.
  xxh64
    :: t
    -- ^ Data to hash
    -> Word64
    -- ^ Seed
    -> Word64
    -- ^ Resulting hash

{-# DEPRECATED XXHash "Use new, XXH3-based functions instead" #-}

instance XXHash BS.ByteString where
  xxh32 bs seed = fromIntegral . unsafePerformIO . useBS bs $
    \ptr len -> c_xxh32 ptr len (fromIntegral seed)

  xxh64 bs seed = fromIntegral . unsafePerformIO . useBS bs $
    \ptr len -> c_xxh64 ptr len (fromIntegral seed)

instance XXHash BL.ByteString where
  xxh32 bs seed = fromIntegral . unsafePerformIO $
    allocaXXH32State $ \state -> do
      c_xxh32_reset state (fromIntegral seed)
      mapM_ (update state) (BL.toChunks bs)
      c_xxh32_digest state
    where
      update state bs' = useBS bs' $ c_xxh32_update state

  xxh64 bs seed = fromIntegral . unsafePerformIO $
    allocaXXH64State $ \state -> do
      c_xxh64_reset state (fromIntegral seed)
      mapM_ (update state) (BL.toChunks bs)
      c_xxh64_digest state
    where
      update state bs' = useBS bs' $ c_xxh64_update state

newtype XXH3 a = XXH3 {unXXH3 :: a}
  deriving (Eq, Ord, Show)

instance Hashable (XXH3 BS.ByteString) where
  hashWithSalt salt (XXH3 bs) = fromIntegral . unsafePerformIO . useBS bs $
    \ptr len ->
      (if len < 1000000 then c_xxh3_64bits_withSeed else c_xxh3_64bits_withSeed_safe)
        ptr
        len
        (fromIntegral salt)

instance Hashable (XXH3 BL.ByteString) where
  hashWithSalt salt (XXH3 bs) = fromIntegral . unsafePerformIO $
    allocaXXH3State $ \state -> do
      c_xxh3_64bits_reset_withSeed state (fromIntegral salt)
      mapM_ (update state) (BL.toChunks bs)
      c_xxh3_64bits_digest state
    where
      update state bs' = useBS bs' $ \ptr len ->
        (if len < 1000000 then c_xxh3_64bits_update else c_xxh3_64bits_update_safe)
          state
          ptr
          len

instance Hashable (XXH3 A.ByteArray) where
  hashWithSalt salt (XXH3 ba@(A.ByteArray ba#)) =
    fromIntegral . unsafePerformIO . useBA ba $
      \ptr len ->
        ( if len < 1000000 || not (isTrue# (isByteArrayPinned# ba#))
            then c_xxh3_64bits_withSeed
            else c_xxh3_64bits_withSeed_safe
        )
          ptr
          len
          (fromIntegral salt)
