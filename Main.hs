{-# LANGUAGE TransformListComp #-}
module Main where

import Control.Monad
import Control.Monad.Trans.Resource
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Binary.Builder as Builder
import Data.Conduit
import Data.Conduit.Combinators as CL
import Data.Conduit.List
import Data.Set as Set
import Data.Csv as Csv
import Data.Csv.Builder as Csv
import Data.Foldable
import Data.Functor
import Data.Function
import Data.Traversable
import qualified Data.Map as Map
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Aeson
import Data.Either
import Data.List
import Data.Maybe as M
import GhcBuildPhase
import GhcFile
import GHC.Exts
import qualified Data.Vector as V
import System.Environment
import System.FilePath
import Prelude hiding (mapM_, print)
import qualified Prelude

main :: IO ()
main = do
  [dir] <- getArgs

  files <- findDumpTimings dir

  let ( files_failed,
        files_parsed)
        = partitionEithers $ files <&> \file ->
            case stripPrefix dir file of
              Nothing -> Left file
              Just x -> case splitDirectories x of
                ("/": "build": hostOs: ghcVersion: packageName: "build": modulePath) -> Right GhcFile{..}
                _ -> Left file

  unless (Prelude.null files_failed) $ do
    Prelude.putStrLn "Warning, some files are failed to be parsed"
    Prelude.print files_failed

  -- Output all files in json form for later analysis.
  results <- for files_parsed $ \f -> do
    steps <- fmap parsePhases $ T.readFile (rebuildFilePath dir f)
    encodeFile (output </> rebuildPlainPath f <.> "json") steps
    let bs = encodeDefaultOrderedByName steps
    BSL.writeFile (output </> rebuildPlainPath f <.> "csv") bs
    pure (f, steps)

  let stats_by_package = Map.fromListWith (<>)
        [ (packageName, Map.singleton (joinPath modulePath) steps)
        | (GhcFile{..}, steps) <- results
        ]
  encodeFile (output </> "stats_by_package" <.> "json") stats_by_package
  for_ (Map.toList stats_by_package) $ \(package, stat) -> do
    let headers = Set.toList $ Set.fromList 
           [ T.encodeUtf8 phaseName
           | (_, steps) <- Map.toList stat
           , Phase{..} <- steps
           ]
    let bs = Csv.encodeHeader (V.fromList ("module": "total": headers))
             <> mconcat
                [ Csv.encodeRecord
                $ module_name
                : show total
                : Prelude.map (\n -> maybe "" show $ Map.lookup n by_phase) headers
                | (module_name, steps) <- Map.toList stat
                , let total = Prelude.sum [phaseTime | Phase{..} <- steps]
                , let by_phase =  Map.fromListWith (+) 
                                    [(T.encodeUtf8 phaseName, phaseTime)
                                    | Phase{..} <- steps
                                    ]
                , then sortWith by (Down total)
                ]
    BSL.writeFile (output </> package <.> "csv")
       $ Builder.toLazyByteString bs
  -- Prelude.print byPackage
  where
    output = "./tmp"

-- | Find all files that are related to the dump timings.
--
-- XXX: this method is not effective enough as it eagerly builds a list of FilePath
findDumpTimings :: String -> IO [FilePath]
findDumpTimings input = do
  runResourceT $ runConduit $ sourceDirectoryDeep False input
    .| CL.filter (\x -> x `endsWith` ".dump-timings")
    .| consume
  where
    endsWith x y = (reverse y) `isPrefixOf` (reverse x)