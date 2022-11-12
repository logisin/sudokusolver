{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_sudokusolver (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/dso/.cabal/bin"
libdir     = "/home/dso/.cabal/lib/x86_64-linux-ghc-8.10.7/sudokusolver-0.1.0.0-inplace-sudokusolver"
dynlibdir  = "/home/dso/.cabal/lib/ghc-8.10.7/sudokusolver-0.1.0.0-inplace-sudokusolver"
datadir    = "/home/dso/.cabal/share/x86_64-linux-ghc-8.10.7/sudokusolver-0.1.0.0"
libexecdir = "/home/dso/.cabal/libexec/x86_64-linux-ghc-8.10.7/sudokusolver-0.1.0.0"
sysconfdir = "/home/dso/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "sudokusolver_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "sudokusolver_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "sudokusolver_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "sudokusolver_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "sudokusolver_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "sudokusolver_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
