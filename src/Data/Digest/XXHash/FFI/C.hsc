{-# LANGUAGE CApiFFI                  #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# LANGUAGE UnliftedFFITypes         #-}
-- |
-- Module:      Data.Digest.XXHash.FFI.C
-- Copyright:   (c) 2017 Henri Verroken
-- Licence:     BSD3
-- Maintainer:  Henri Verroken <henriverroken@gmail.com
-- Stability:   stable
-- Portability: GHC
--
-- This module provides FFI imports to the C reference library at
-- <https://github.com/Cyan4973/xxHash>.
--
-- This binding keeps the intermediate state for stream processing in an
-- 'MutableByteArray#' on the managed GHC heap. All foreign imports use unsafe
-- call semantics. Therefore, it is possible to use either unpinned or pinned
-- 'MutableByteArray#' since GHC's garbage collector doesn't move the either
-- kind during an unsafe foreign call. However GHCi <8.4 may replace unsafe
-- foreign calls with safe foreign calls in the bytecode
-- interpreter. Consequently, unpinned 'MutableByteArray#s' may be moved by the
-- garbage collector during foreign calls which obviously breaks this code. So
-- extra care should be taken when loading this code into the bytecode
-- interpreter.
module Data.Digest.XXHash.FFI.C (
  -- * Direct calculation
  c_xxh3_64bits_withSeed
, c_xxh3_64bits_withSeed_safe
, c_xxh64
, c_xxh64_safe
, c_xxh32
, c_xxh32_safe

  -- * 32-bit state functions
, XXH32State
, allocaXXH32State
, c_xxh32_copyState
, c_xxh32_reset
, c_xxh32_update
, c_xxh32_update_safe
, c_xxh32_digest

  -- * 64-bit state functions
, XXH64State
, allocaXXH64State
, c_xxh64_copyState
, c_xxh64_reset
, c_xxh64_update
, c_xxh64_update_safe
, c_xxh64_digest
  
  -- * XXH3 state functions
, XXH3State
, allocaXXH3State
, c_xxh3_copyState
, c_xxh3_64bits_reset_withSeed
, c_xxh3_64bits_update
, c_xxh3_64bits_update_safe
, c_xxh3_64bits_digest
) where

-- Define XXH_STATIC_LINKING_ONLY to expose the definition of the state structs.
-- We can then get the size of them and allocate them on the managed GHC heap.
#define XXH_STATIC_LINKING_ONLY
#include "xxhash.h"

import Foreign.C.Types
import Foreign.Ptr       (Ptr)
import GHC.Exts          (Int(..), RealWorld,
                          MutableByteArray##, newByteArray##)
import GHC.IO            (IO(IO))

-- | @since 0.3
foreign import capi unsafe "xxhash.h XXH3_64bits_withSeed" c_xxh3_64bits_withSeed ::
    Ptr a      -- ^ 'Ptr' to the input buffer
 -> CSize      -- ^ Buffer length
 -> CULLong    -- ^ Seed
 -> IO CULLong -- ^ Resulting hash

-- | Same as 'c_xxh3_64bits_withSeed', but using @safe@ modifier.
-- Prefer this function to hash large data in multithreaded environment.
--
-- @since 0.3
foreign import capi safe "xxhash.h XXH3_64bits_withSeed" c_xxh3_64bits_withSeed_safe ::
    Ptr a      -- ^ 'Ptr' to the input buffer
 -> CSize      -- ^ Buffer length
 -> CULLong    -- ^ Seed
 -> IO CULLong -- ^ Resulting hash

foreign import capi unsafe "xxhash.h XXH64" c_xxh64 ::
    Ptr a      -- ^ 'Ptr' to the input buffer
 -> CSize      -- ^ Buffer length
 -> CULLong    -- ^ Seed
 -> IO CULLong -- ^ Resulting hash

-- | Same as 'c_xxh64', but using @safe@ modifier.
-- Prefer this function to hash large data in multithreaded environment.
--
-- @since 0.3
foreign import capi safe "xxhash.h XXH64" c_xxh64_safe ::
    Ptr a      -- ^ 'Ptr' to the input buffer
 -> CSize      -- ^ Buffer length
 -> CULLong    -- ^ Seed
 -> IO CULLong -- ^ Resulting hash

foreign import capi unsafe "xxhash.h XXH32" c_xxh32 ::
    Ptr a      -- ^ 'Ptr' to the input buffer
 -> CSize      -- ^ Buffer length
 -> CUInt      -- ^ Seed
 -> IO CUInt   -- ^ Resulting hash

-- | Same as 'c_xxh32', but using @safe@ modifier.
-- Prefer this function to hash large data in multithreaded environment.
--
-- @since 0.3
foreign import capi safe "xxhash.h XXH32" c_xxh32_safe ::
    Ptr a      -- ^ 'Ptr' to the input buffer
 -> CSize      -- ^ Buffer length
 -> CUInt      -- ^ Seed
 -> IO CUInt   -- ^ Resulting hash

-- | Intermediate state for computing a XXH32 using segmentation or streams.
type XXH32State = MutableByteArray## RealWorld

foreign import capi unsafe "xxhash.h XXH32_copyState" c_xxh32_copyState ::
    XXH32State     -- ^ Destination
 -> XXH32State     -- ^ Source
 -> IO ()

foreign import capi unsafe "xxhash.h XXH32_reset" c_xxh32_reset ::
    XXH32State     -- ^ The state to reset
 -> CUInt          -- ^ The initial seed
 -> IO ()

foreign import capi unsafe "xxhash.h XXH32_update" c_xxh32_update ::
    XXH32State     -- ^ The state to update
 -> Ptr a          -- ^ 'Ptr' to the input buffer
 -> CSize          -- ^ Buffer length
 -> IO ()

-- | Same as 'c_xxh32_update', but using @safe@ modifier.
-- Prefer this function to hash large data in multithreaded environment.
--
-- @since 0.3
foreign import capi safe "xxhash.h XXH32_update" c_xxh32_update_safe ::
    XXH32State     -- ^ The state to update
 -> Ptr a          -- ^ 'Ptr' to the input buffer
 -> CSize          -- ^ Buffer length
 -> IO ()

foreign import capi unsafe "xxhash.h XXH32_digest" c_xxh32_digest ::
    XXH32State     -- ^ The state to digest
 -> IO CUInt       -- ^ Resulting hash

-- | Intermediate state for computing a XXH64 using segmentation or streams.
type XXH64State = MutableByteArray## RealWorld

foreign import capi unsafe "xxhash.h XXH64_copyState" c_xxh64_copyState ::
    XXH64State     -- ^ Destination
 -> XXH64State     -- ^ Source
 -> IO ()

foreign import capi unsafe "xxhash.h XXH64_reset" c_xxh64_reset ::
    XXH64State     -- ^ The state to reset
 -> CULLong        -- ^ The initial seed
 -> IO ()

foreign import capi unsafe "xxhash.h XXH64_update" c_xxh64_update ::
    XXH64State     -- ^ The state to update
 -> Ptr a          -- ^ 'Ptr' to the input buffer
 -> CSize          -- ^ Buffer length
 -> IO ()

-- | Same as 'c_xxh64_update', but using @safe@ modifier.
-- Prefer this function to hash large data in multithreaded environment.
--
-- @since 0.3
foreign import capi safe "xxhash.h XXH64_update" c_xxh64_update_safe ::
    XXH64State     -- ^ The state to update
 -> Ptr a          -- ^ 'Ptr' to the input buffer
 -> CSize          -- ^ Buffer length
 -> IO ()

foreign import capi unsafe "xxhash.h XXH64_digest" c_xxh64_digest ::
    XXH64State     -- ^ The state to digest
 -> IO CULLong     -- ^ Resulting hash

-- | Intermediate state for computing a XXH3 using segmentation or streams.
--
-- @since 0.3
type XXH3State = MutableByteArray## RealWorld

-- | @since 0.3
foreign import capi unsafe "xxhash.h XXH3_copyState" c_xxh3_copyState ::
    XXH3State      -- ^ Destination
 -> XXH3State      -- ^ Source
 -> IO ()

-- | @since 0.3
foreign import capi unsafe "xxhash.h XXH3_64bits_reset_withSeed" c_xxh3_64bits_reset_withSeed ::
    XXH3State      -- ^ The state to reset
 -> CULLong        -- ^ The initial seed
 -> IO ()

-- | @since 0.3
foreign import capi unsafe "xxhash.h XXH3_64bits_update" c_xxh3_64bits_update ::
    XXH3State      -- ^ The state to update
 -> Ptr a          -- ^ 'Ptr' to the input buffer
 -> CSize          -- ^ Buffer length
 -> IO ()

-- | Same as 'c_xxh3_64bits_update', but using @safe@ modifier.
-- Prefer this function to hash large data in multithreaded environment.
--
-- @since 0.3
foreign import capi safe "xxhash.h XXH3_64bits_update" c_xxh3_64bits_update_safe ::
    XXH3State      -- ^ The state to update
 -> Ptr a          -- ^ 'Ptr' to the input buffer
 -> CSize          -- ^ Buffer length
 -> IO ()

-- | @since 0.3
foreign import capi unsafe "xxhash.h XXH3_64bits_digest" c_xxh3_64bits_digest ::
    XXH3State      -- ^ The state to digest
 -> IO CULLong     -- ^ Resulting hash

{-# INLINE allocaMutableByteArray #-}
allocaMutableByteArray :: Int -> (MutableByteArray## RealWorld -> IO b) -> IO b
allocaMutableByteArray (I## len) f = IO $ \s0 ->
    case newByteArray## len s0 of { (## s1, mba ##) ->
    case f mba                 of { IO m -> m s1 }}

{-# INLINE allocaXXH32State #-}
-- | 'allocaXXH32State' @f@ temporarily allocates a 'XXH32State' and passes it
--   to the function @f@.
allocaXXH32State :: (XXH32State -> IO a) -> IO a
allocaXXH32State = allocaMutableByteArray #{size XXH32_state_t}

{-# INLINE allocaXXH64State #-}
-- | 'allocaXXH64State'  @f@ temporarily allocates a 'XXH64State' and passes it
--   to the function  @f@.
allocaXXH64State :: (XXH64State -> IO a) -> IO a
allocaXXH64State = allocaMutableByteArray #{size XXH64_state_t}

{-# INLINE allocaXXH3State #-}
-- | 'allocaXXH3State' @f@ temporarily allocates a 'XXH3State' and passes it
--   to the function  @f@.
allocaXXH3State :: (XXH3State -> IO a) -> IO a
allocaXXH3State = allocaMutableByteArray #{size XXH3_state_t}
