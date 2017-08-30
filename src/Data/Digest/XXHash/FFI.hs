{-# LANGUAGE ForeignFunctionInterface #-}
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

  -- * C Interface
  -- ** Direct Calculation
, c_xxh64
, c_xxh32

  -- ** 32-bit state functions
, XXH32State
, c_xxh32_createState
, c_xxh32_freeState
, c_xxh32_copyState
, c_xxh32_reset
, c_xxh32_update
, c_xxh32_digest

  -- ** 64-bit state functions
, XXH64State
, c_xxh64_createState
, c_xxh64_freeState
, c_xxh64_copyState
, c_xxh64_reset
, c_xxh64_update
, c_xxh64_digest
) where

import Control.Exception (bracket)

import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Word (Word32, Word64)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Foreign.C
import Foreign.Ptr

import GHC.Exts         (realWorld#)
import GHC.IO           (IO(IO))
import System.IO.Unsafe (unsafePerformIO)

foreign import ccall unsafe "XXH64" c_xxh64 ::
    Ptr a      -- ^ 'Ptr' to the input buffer
 -> CSize      -- ^ Buffer length
 -> CULLong    -- ^ Seed
 -> IO CULLong -- ^ Resulting hash

foreign import ccall unsafe "XXH32" c_xxh32 ::
    Ptr a      -- ^ 'Ptr' to the input buffer
 -> CSize      -- ^ Buffer length
 -> CUInt      -- ^ Seed
 -> IO CUInt   -- ^ Resulting hash

data XXH32State

foreign import ccall unsafe "XXH32_createState" c_xxh32_createState ::
    IO (Ptr XXH32State) -- ^ Pointer to a newly allocated state

foreign import ccall unsafe "XXH32_freeState" c_xxh32_freeState ::
    Ptr XXH32State
 -> IO () -- ^ Free pointer allocated by 'c_xxh32_createState'

foreign import ccall unsafe "XXH32_copyState" c_xxh32_copyState ::
    Ptr XXH32State -- ^ Destination
 -> Ptr XXH32State -- ^ Source
 -> IO ()

foreign import ccall unsafe "XXH32_reset" c_xxh32_reset ::
    Ptr XXH32State -- ^ The state to reset
 -> CUInt          -- ^ The initial seed
 -> IO ()

foreign import ccall unsafe "XXH32_update" c_xxh32_update ::
    Ptr XXH32State -- ^ The state to update
 -> Ptr a          -- ^ 'Ptr' to the input buffer
 -> CSize          -- ^ Buffer length
 -> IO ()

foreign import ccall unsafe "XXH32_digest" c_xxh32_digest ::
    Ptr XXH32State -- ^ The state to digest
 -> IO CUInt       -- ^ Resulting hash

data XXH64State

foreign import ccall unsafe "XXH64_createState" c_xxh64_createState ::
    IO (Ptr XXH64State) -- ^ Pointer to a newly allocated state

foreign import ccall unsafe "XXH64_freeState" c_xxh64_freeState ::
    Ptr XXH64State
 -> IO () -- ^ Free pointer allocated by 'c_xxh64_createState'

foreign import ccall unsafe "XXH64_copyState" c_xxh64_copyState ::
    Ptr XXH64State -- ^ Destination
 -> Ptr XXH64State -- ^ Source
 -> IO ()

foreign import ccall unsafe "XXH64_reset" c_xxh64_reset ::
    Ptr XXH64State -- ^ The state to reset
 -> CULLong        -- ^ The initial seed
 -> IO ()

foreign import ccall unsafe "XXH64_update" c_xxh64_update ::
    Ptr XXH64State -- ^ The state to update
 -> Ptr a          -- ^ 'Ptr' to the input buffer
 -> CSize          -- ^ Buffer length
 -> IO ()

foreign import ccall unsafe "XXH64_digest" c_xxh64_digest ::
    Ptr XXH64State -- ^ The state to digest
 -> IO CULLong     -- ^ Resulting hash

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
        bracket c_xxh32_createState
                c_xxh32_freeState $ \state -> do
                    c_xxh32_reset state (fromIntegral seed)
                    mapM_ (update state) (BL.toChunks bs)
                    c_xxh32_digest state
      where
        update state bs' = use bs' $ c_xxh32_update state

    xxh64 bs seed = fromIntegral . unsafePerformIO $
        bracket c_xxh64_createState
                c_xxh64_freeState $ \state -> do
                    c_xxh64_reset state (fromIntegral seed)
                    mapM_ (update state) (BL.toChunks bs)
                    c_xxh64_digest state
      where
        update state bs' = use bs' $ c_xxh64_update state

{-# SPECIALIZE xxh32 :: BL.ByteString -> Word32 -> Word32 #-}
{-# SPECIALIZE xxh64 :: BL.ByteString -> Word64 -> Word64 #-}
