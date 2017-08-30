{-# LANGUAGE MagicHash     #-}
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
  -- * Interface
  XXHash(..)
) where

import Data.Digest.XXHash.FFI.C

import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Word (Word32, Word64)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Foreign.C
import GHC.Exts         (realWorld#)
import GHC.IO           (IO(IO))
import System.IO.Unsafe (unsafePerformIO)

{-# INLINE inlinePerformIO #-}
inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r

{-# INLINE use #-}
use :: BS.ByteString -> (CString -> CSize -> IO a) -> IO a
use bs k = unsafeUseAsCStringLen bs $ \(ptr,len) -> k ptr (fromIntegral len)

-- | Class for hashable data types.
--
-- Not that all standard instances are specialized using the @SPECIALIZE@
-- pragma.
class XXHash t where
    -- | Calculate the 32-bit xxHash using a given seed.
    xxh32 :: t      -- ^ Data to hash
          -> Word32 -- ^ Seed
          -> Word32 -- ^ Resulting hash

    -- | Calculate the 64-bit xxHash using a given seed.
    xxh64 :: t      -- ^ Data to hash
          -> Word64 -- ^ Seed
          -> Word64 -- ^ Resulting hash


instance XXHash BS.ByteString where
    xxh32 bs seed = fromIntegral . inlinePerformIO . use bs $
        \ptr len -> c_xxh32 ptr len (fromIntegral seed)

    xxh64 bs seed = fromIntegral . inlinePerformIO . use bs $
        \ptr len -> c_xxh64 ptr len (fromIntegral seed)

{-# SPECIALIZE xxh32 :: BS.ByteString -> Word32 -> Word32 #-}
{-# SPECIALIZE xxh64 :: BS.ByteString -> Word64 -> Word64 #-}

instance XXHash BL.ByteString where
    xxh32 bs seed = fromIntegral . unsafePerformIO $
        allocaXXH32State $ \state -> do
            c_xxh32_reset state (fromIntegral seed)
            mapM_ (update state) (BL.toChunks bs)
            c_xxh32_digest state
      where
        update state bs' = use bs' $ c_xxh32_update state

    xxh64 bs seed = fromIntegral . unsafePerformIO $
        allocaXXH64State $ \state -> do
            c_xxh64_reset state (fromIntegral seed)
            mapM_ (update state) (BL.toChunks bs)
            c_xxh64_digest state
      where
        update state bs' = use bs' $ c_xxh64_update state

{-# SPECIALIZE xxh32 :: BL.ByteString -> Word32 -> Word32 #-}
{-# SPECIALIZE xxh64 :: BL.ByteString -> Word64 -> Word64 #-}
