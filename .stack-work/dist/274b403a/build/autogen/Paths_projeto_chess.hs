{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_projeto_chess (
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

bindir     = "C:\\Users\\nome\\Desktop\\Haskell\\projeto-chess\\.stack-work\\install\\20dd86c1\\bin"
libdir     = "C:\\Users\\nome\\Desktop\\Haskell\\projeto-chess\\.stack-work\\install\\20dd86c1\\lib\\x86_64-windows-ghc-8.10.4\\projeto-chess-0.1.0.0-FyXZKk3L2kM8AwImTEZ3FY"
dynlibdir  = "C:\\Users\\nome\\Desktop\\Haskell\\projeto-chess\\.stack-work\\install\\20dd86c1\\lib\\x86_64-windows-ghc-8.10.4"
datadir    = "C:\\Users\\nome\\Desktop\\Haskell\\projeto-chess\\.stack-work\\install\\20dd86c1\\share\\x86_64-windows-ghc-8.10.4\\projeto-chess-0.1.0.0"
libexecdir = "C:\\Users\\nome\\Desktop\\Haskell\\projeto-chess\\.stack-work\\install\\20dd86c1\\libexec\\x86_64-windows-ghc-8.10.4\\projeto-chess-0.1.0.0"
sysconfdir = "C:\\Users\\nome\\Desktop\\Haskell\\projeto-chess\\.stack-work\\install\\20dd86c1\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "projeto_chess_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "projeto_chess_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "projeto_chess_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "projeto_chess_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "projeto_chess_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "projeto_chess_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
