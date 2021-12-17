{-# LINE 1 "Data/Memory/MemMap/Posix.hsc" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Memory.MemMap.Posix
-- Copyright   :  (c) Vincent Hanquez 2014
-- License     :  BSD-style
--
-- Maintainer  :  Vincent Hanquez
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- Functions defined by the POSIX standards for manipulating memory maps
--
-- When a function that calls an underlying POSIX function fails, the errno
-- code is converted to an 'IOError' using 'Foreign.C.Error.errnoToIOError'.
-- For a list of which errno codes may be generated, consult the POSIX
-- documentation for the underlying function.
--
-----------------------------------------------------------------------------




{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
module Data.Memory.MemMap.Posix
    ( memoryMap
    , memoryUnmap
    , memoryAdvise
    , memoryLock
    , memoryUnlock
    , memoryProtect
    , memorySync
    -- * Flags types
    , MemoryMapFlag(..)
    , MemoryProtection(..)
    , MemoryAdvice(..)
    , MemorySyncFlag(..)
    -- * system page size
    , sysconfPageSize
    ) where

import System.Posix.Types
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.Error
import Data.Bits

foreign import ccall unsafe "mmap"
    c_mmap :: Ptr a -> CSize -> CInt -> CInt -> CInt -> COff -> IO (Ptr a)

foreign import ccall unsafe "munmap"
    c_munmap :: Ptr a -> CSize -> IO CInt


{-# LINE 55 "Data/Memory/MemMap/Posix.hsc" #-}
foreign import ccall unsafe "posix_madvise"
    c_madvise :: Ptr a -> CSize -> CInt -> IO CInt

{-# LINE 61 "Data/Memory/MemMap/Posix.hsc" #-}

foreign import ccall unsafe "msync"
    c_msync :: Ptr a -> CSize -> CInt -> IO CInt

foreign import ccall unsafe "mprotect"
    c_mprotect :: Ptr a -> CSize -> CInt -> IO CInt


{-# LINE 69 "Data/Memory/MemMap/Posix.hsc" #-}
foreign import ccall unsafe "mlock"
    c_mlock :: Ptr a -> CSize -> IO CInt

{-# LINE 75 "Data/Memory/MemMap/Posix.hsc" #-}


{-# LINE 77 "Data/Memory/MemMap/Posix.hsc" #-}
foreign import ccall unsafe "munlock"
    c_munlock :: Ptr a -> CSize -> IO CInt

{-# LINE 83 "Data/Memory/MemMap/Posix.hsc" #-}

foreign import ccall unsafe "sysconf"
    c_sysconf :: CInt -> CLong

-- | Mapping flag
data MemoryMapFlag =
      MemoryMapShared  -- ^ memory changes are shared between process
    | MemoryMapPrivate -- ^ memory changes are private to process
    deriving (Show,Read,Eq)

-- | Memory protection
data MemoryProtection =
      MemoryProtectionNone
    | MemoryProtectionRead
    | MemoryProtectionWrite
    | MemoryProtectionExecute
    deriving (Show,Read,Eq)

-- | Advice to put on memory.
--
-- only define the posix one.
data MemoryAdvice =
      MemoryAdviceNormal     -- ^ no specific advice, the default.
    | MemoryAdviceRandom     -- ^ Expect page references in random order. No readahead should occur.
    | MemoryAdviceSequential -- ^ Expect page references in sequential order. Page should be readahead aggressively.
    | MemoryAdviceWillNeed   -- ^ Expect access in the near future. Probably a good idea to readahead early
    | MemoryAdviceDontNeed   -- ^ Do not expect access in the near future.
    deriving (Show,Read,Eq)

-- | Memory synchronization flags
data MemorySyncFlag =
      MemorySyncAsync      -- ^ perform asynchronous write.
    | MemorySyncSync       -- ^ perform synchronous write.
    | MemorySyncInvalidate -- ^ invalidate cache data.
    deriving (Show,Read,Eq)

cvalueOfMemoryProts :: [MemoryProtection] -> CInt
cvalueOfMemoryProts = foldl (.|.) 0 . map toProt
  where toProt :: MemoryProtection -> CInt
        toProt MemoryProtectionNone    = (0)
{-# LINE 123 "Data/Memory/MemMap/Posix.hsc" #-}
        toProt MemoryProtectionRead    = (1)
{-# LINE 124 "Data/Memory/MemMap/Posix.hsc" #-}
        toProt MemoryProtectionWrite   = (2)
{-# LINE 125 "Data/Memory/MemMap/Posix.hsc" #-}
        toProt MemoryProtectionExecute = (4)
{-# LINE 126 "Data/Memory/MemMap/Posix.hsc" #-}

cvalueOfMemorySync :: [MemorySyncFlag] -> CInt
cvalueOfMemorySync = foldl (.|.) 0 . map toSync
  where toSync MemorySyncAsync      = (1)
{-# LINE 130 "Data/Memory/MemMap/Posix.hsc" #-}
        toSync MemorySyncSync       = (16)
{-# LINE 131 "Data/Memory/MemMap/Posix.hsc" #-}
        toSync MemorySyncInvalidate = (2)
{-# LINE 132 "Data/Memory/MemMap/Posix.hsc" #-}

-- | Map pages of memory.
--
-- If fd is present, this memory will represent the file associated.
-- Otherwise, the memory will be an anonymous mapping.
--
-- use 'mmap'
memoryMap :: Maybe (Ptr a)      -- ^ The address to map to if MapFixed is used.
          -> CSize              -- ^ The length of the mapping
          -> [MemoryProtection] -- ^ the memory protection associated with the mapping
          -> MemoryMapFlag      -- ^ 
          -> Maybe Fd
          -> COff
          -> IO (Ptr a)
memoryMap initPtr sz prots flag mfd off =
    throwErrnoIf (== m1ptr) "mmap" (c_mmap (maybe nullPtr id initPtr) sz cprot cflags fd off)
  where m1ptr  = nullPtr `plusPtr` (-1)
        fd     = maybe (-1) (\(Fd v) -> v) mfd
        cprot  = cvalueOfMemoryProts prots
        cflags = maybe cMapAnon (const 0) mfd
             .|. maybe 0 (const cMapFixed) initPtr
             .|. toMapFlag flag


{-# LINE 156 "Data/Memory/MemMap/Posix.hsc" #-}
        cMapAnon  = (4096)
{-# LINE 157 "Data/Memory/MemMap/Posix.hsc" #-}

{-# LINE 160 "Data/Memory/MemMap/Posix.hsc" #-}
        cMapFixed = (16)
{-# LINE 161 "Data/Memory/MemMap/Posix.hsc" #-}

        toMapFlag MemoryMapShared  = (1)
{-# LINE 163 "Data/Memory/MemMap/Posix.hsc" #-}
        toMapFlag MemoryMapPrivate = (2)
{-# LINE 164 "Data/Memory/MemMap/Posix.hsc" #-}

-- | Unmap pages of memory
--
-- use 'munmap'
memoryUnmap :: Ptr a -> CSize -> IO ()
memoryUnmap ptr sz = throwErrnoIfMinus1_ "munmap" (c_munmap ptr sz)

-- | give advice to the operating system about use of memory
--
-- call 'madvise'
memoryAdvise :: Ptr a -> CSize -> MemoryAdvice -> IO ()
memoryAdvise ptr sz adv = throwErrnoIfMinus1_ "madvise" (c_madvise ptr sz cadv)
  where cadv = toAdvice adv

{-# LINE 178 "Data/Memory/MemMap/Posix.hsc" #-}
        toAdvice MemoryAdviceNormal = (0)
{-# LINE 179 "Data/Memory/MemMap/Posix.hsc" #-}
        toAdvice MemoryAdviceRandom = (1)
{-# LINE 180 "Data/Memory/MemMap/Posix.hsc" #-}
        toAdvice MemoryAdviceSequential = (2)
{-# LINE 181 "Data/Memory/MemMap/Posix.hsc" #-}
        toAdvice MemoryAdviceWillNeed = (3)
{-# LINE 182 "Data/Memory/MemMap/Posix.hsc" #-}
        toAdvice MemoryAdviceDontNeed = (4)
{-# LINE 183 "Data/Memory/MemMap/Posix.hsc" #-}

{-# LINE 190 "Data/Memory/MemMap/Posix.hsc" #-}

-- | lock a range of process address space
--
-- call 'mlock'
memoryLock :: Ptr a -> CSize -> IO ()
memoryLock ptr sz = throwErrnoIfMinus1_ "mlock" (c_mlock ptr sz)

-- | unlock a range of process address space
--
-- call 'munlock'
memoryUnlock :: Ptr a -> CSize -> IO ()
memoryUnlock ptr sz = throwErrnoIfMinus1_ "munlock" (c_munlock ptr sz)

-- | set protection of memory mapping
--
-- call 'mprotect'
memoryProtect :: Ptr a -> CSize -> [MemoryProtection] -> IO ()
memoryProtect ptr sz prots = throwErrnoIfMinus1_ "mprotect" (c_mprotect ptr sz cprot)
  where cprot = cvalueOfMemoryProts prots

-- | memorySync synchronize memory with physical storage.
--
-- On an anonymous mapping this function doesn't have any effect.
-- call 'msync'
memorySync :: Ptr a -> CSize -> [MemorySyncFlag] -> IO ()
memorySync ptr sz flags = throwErrnoIfMinus1_ "msync" (c_msync ptr sz cflags)
  where cflags = cvalueOfMemorySync flags

-- | Return the operating system page size.
-- 
-- call 'sysconf'
sysconfPageSize :: Int
sysconfPageSize = fromIntegral $ c_sysconf (29)
{-# LINE 223 "Data/Memory/MemMap/Posix.hsc" #-}
