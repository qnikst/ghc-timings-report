{-# LANGUAGE StrictData #-}
module GhcFile
  ( GhcFile(..)
  , rebuildFilePath
  , rebuildPlainPath
  ) where

import Data.Aeson
import Data.List
import GHC.Generics (Generic)
import System.FilePath

-- | Representation of the file in the filesystem structure.
-- 
-- This file follows pattern used in cabal build and may differ for
-- other build systems. I don't care about those, but patches are welcome.
data GhcFile = GhcFile
  { hostOs :: String -- ^ Host
  , ghcVersion :: String
  , packageName :: String
  , modulePath :: [String]
  }
  deriving (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Build path to the file in the file system based on prefix and 'GhcFile'
--
-- It looks terrible, seems a wrong abstraction is here.
rebuildFilePath :: FilePath -> GhcFile -> FilePath
rebuildFilePath base GhcFile{..} =
  base </> "build" </> hostOs </> ghcVersion </> packageName </> "build" </> joinPath modulePath

-- | Convert 'GhcFile' into plain filename that we use in our report storage.
rebuildPlainPath :: GhcFile -> FilePath
rebuildPlainPath GhcFile{..} = 
   intercalate "--" $ [hostOs, ghcVersion, packageName] ++ modulePath
