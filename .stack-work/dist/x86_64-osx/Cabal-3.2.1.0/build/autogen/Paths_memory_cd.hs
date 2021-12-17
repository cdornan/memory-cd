{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_memory_cd (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,16,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Volumes/hs/memory-cd/.stack-work/install/x86_64-osx/8b2204cb5f4bb27d4b9b5d28e2af7c9bc540a4245e56767d57e01858784475bf/8.10.7/bin"
libdir     = "/Volumes/hs/memory-cd/.stack-work/install/x86_64-osx/8b2204cb5f4bb27d4b9b5d28e2af7c9bc540a4245e56767d57e01858784475bf/8.10.7/lib/x86_64-osx-ghc-8.10.7/memory-cd-0.16.0-A8bDME6Wtr6Kqdz2qqfFGk"
dynlibdir  = "/Volumes/hs/memory-cd/.stack-work/install/x86_64-osx/8b2204cb5f4bb27d4b9b5d28e2af7c9bc540a4245e56767d57e01858784475bf/8.10.7/lib/x86_64-osx-ghc-8.10.7"
datadir    = "/Volumes/hs/memory-cd/.stack-work/install/x86_64-osx/8b2204cb5f4bb27d4b9b5d28e2af7c9bc540a4245e56767d57e01858784475bf/8.10.7/share/x86_64-osx-ghc-8.10.7/memory-cd-0.16.0"
libexecdir = "/Volumes/hs/memory-cd/.stack-work/install/x86_64-osx/8b2204cb5f4bb27d4b9b5d28e2af7c9bc540a4245e56767d57e01858784475bf/8.10.7/libexec/x86_64-osx-ghc-8.10.7/memory-cd-0.16.0"
sysconfdir = "/Volumes/hs/memory-cd/.stack-work/install/x86_64-osx/8b2204cb5f4bb27d4b9b5d28e2af7c9bc540a4245e56767d57e01858784475bf/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "memory_cd_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "memory_cd_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "memory_cd_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "memory_cd_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "memory_cd_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "memory_cd_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
