{-# LANGUAGE CPP #-}
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
import qualified Data.Text.Array as TA
import qualified Data.Text.Internal as TS
import qualified Data.Text.Lazy as TL
import Data.Word (Word32, Word64)
import Foreign.C
import Foreign.Ptr (plusPtr)
import GHC.Exts (ByteArray#, Int (..), Ptr (..), byteArrayContents#, isByteArrayPinned#, isTrue#, sizeofByteArray#)
import System.IO.Unsafe (unsafePerformIO)

{-# INLINE useBS #-}
useBS :: BS.ByteString -> (CString -> CSize -> IO a) -> IO a
useBS bs k = unsafeUseAsCStringLen bs $ \(ptr, len) -> k ptr (fromIntegral len)

{-# INLINE useBA #-}
useBA :: A.ByteArray -> (CString -> CSize -> IO a) -> IO a
useBA (A.ByteArray ba#) k = k (Ptr (byteArrayContents# ba#)) (fromIntegral (I# (sizeofByteArray# ba#)))

{-# INLINE isPinnedBA #-}
isPinnedBA :: A.ByteArray -> Bool
isPinnedBA (A.ByteArray ba#) = isTrue# (isByteArrayPinned# ba#)

{-# INLINE useTS #-}
useTS :: TS.Text -> (CString -> CSize -> IO a) -> IO a
useTS ts@(TS.Text _ off len) k =
  k
    (Ptr (byteArrayContents# (textArray ts)) `plusPtr` (off * textMult))
    (fromIntegral (len * textMult))

{-# INLINE isPinnedTS #-}
isPinnedTS :: TS.Text -> Bool
isPinnedTS ts = isTrue# (isByteArrayPinned# (textArray ts))

{-# INLINE textArray #-}
textArray :: TS.Text -> ByteArray#
#if MIN_VERSION_text(2,0,0)
textArray (TS.Text (TA.ByteArray ba#) _ _) = ba#
#else
textArray (TS.Text (TA.Array ba#) _ _) = ba#
#endif

{-# INLINE textMult #-}
textMult :: Int
#if MIN_VERSION_text(2,0,0)
textMult = 1
#else
textMult = 2
#endif

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
  hashWithSalt salt (XXH3 ba) =
    fromIntegral . unsafePerformIO . useBA ba $
      \ptr len ->
        ( if len < 1000000 || not (isPinnedBA ba)
            then c_xxh3_64bits_withSeed
            else c_xxh3_64bits_withSeed_safe
        )
          ptr
          len
          (fromIntegral salt)

instance Hashable (XXH3 TS.Text) where
  hashWithSalt salt (XXH3 ts) = fromIntegral . unsafePerformIO . useTS ts $
    \ptr len ->
      ( if len < 1000000 || not (isPinnedTS ts)
          then c_xxh3_64bits_withSeed
          else c_xxh3_64bits_withSeed_safe
      )
        ptr
        len
        (fromIntegral salt)

instance Hashable (XXH3 TL.Text) where
  hashWithSalt salt (XXH3 ts) = fromIntegral . unsafePerformIO $
    allocaXXH3State $ \state -> do
      c_xxh3_64bits_reset_withSeed state (fromIntegral salt)
      mapM_ (update state) (TL.toChunks ts)
      c_xxh3_64bits_digest state
    where
      update state ts' = useTS ts' $ \ptr len ->
        ( if len < 1000000 || not (isPinnedTS ts')
            then c_xxh3_64bits_update
            else c_xxh3_64bits_update_safe
        )
          state
          ptr
          len
