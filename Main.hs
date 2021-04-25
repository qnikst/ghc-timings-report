{-# LANGUAGE TransformListComp #-}
module Main where

import Control.Monad
import Control.Monad.Trans.Resource
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Binary.Builder as Builder
import qualified Data.ByteString as B
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
import qualified Data.Text as T
import Data.Aeson
import Data.Either
import Data.List
import Data.Maybe as M
import GhcBuildPhase
import GhcFile
import GHC.Exts
import qualified Data.Vector as V
import Report
import System.Directory(copyFile, createDirectoryIfMissing)
import System.Environment
import System.FilePath
import TextShow
import Text.Blaze (Markup)
import Text.Blaze.Renderer.Utf8 (renderMarkupToByteStringIO)
import Prelude hiding (mapM_, print)
import qualified Prelude

main :: IO ()
main = do
  [dir] <- getArgs
  createDirectoryIfMissing True "./tmp"

  files <- findDumpTimings dir

  let ( files_failed,
        files_parsed)
        = partitionEithers $ files <&> \file ->
            case stripPrefix dir file of
              Nothing -> Left file
              Just x -> case splitDirectories x of
                ("/" : "build" : hostOs : ghcVersion : packageName : componentType : subComponent : "build" : modulePath) ->
                  let srcFilePath = file
                  in Right GhcFile{..}
                ("/": "dist": hostOs : _cabalVersion : "build": modulePath) ->
                  -- FIXME: should be retrieved from stack somehow
                  let ghcVersion = "<GHC version>"
                      packageName = "<Package name>"
                      componentType = ""
                      subComponent = ""
                      srcFilePath = file
                  in Right GhcFile{..}
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
    mkHtmlFile (output </> rebuildPlainPath f <.> "html")
      $ Report.moduleTable f steps
    pure (f, steps)

  let stats_by_package = Map.fromListWith (<>)
        [ (packageName, Map.singleton GhcFile{..} steps)
        | (GhcFile{..}, steps) <- results
        ]
  -- FIXME: put this file back later
  -- encodeFile (output </> "stats_by_package" <.> "json") stats_by_package
  for_ (Map.toList stats_by_package) $ \(package, stat) -> do
    let headers = Set.toList $ Set.fromList 
           [ phaseName
           | (_, steps) <- Map.toList stat
           , Phase{..} <- steps
           ]
    let rows = [ ( GhcFile{..}
                 , total
                 , Prelude.map (\n -> Map.lookup n by_phase) headers)
               | (GhcFile{..}, steps) <- Map.toList stat
               , let total = Prelude.sum [phaseTime | Phase{..} <- steps]
               , let by_phase =  Map.fromListWith (+) 
                                    [ (phaseName, phaseTime)
                                    | Phase{..} <- steps
                                    ]
               , then sortWith by (Down total)
               ]
    mkHtmlFile ("./tmp/" <> package <> ".html")
      $ Report.packageTable package headers rows
    let bs = Csv.encodeHeader (V.fromList ("module": "total": Prelude.map T.encodeUtf8 headers))
             <> mconcat (Prelude.map Csv.encodeRecord 
               [ Prelude.map T.encodeUtf8 $ T.pack (joinPath modulePath):(showt total):Prelude.map showt cols
               | (GhcFile{..}, total, cols) <- rows
               ])
    BSL.writeFile (output </> package <.> "csv")
       $ Builder.toLazyByteString bs
  -- Prelude.print byPackage
  -- Report.
  mkHtmlFile "./tmp/index.html"
    $ Report.index $ Map.keys stats_by_package
  copyFile "files/main.css" "./tmp/main.css" -- TODO use data files
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

mkHtmlFile :: FilePath -> Markup -> IO ()
mkHtmlFile fn markup = do
  B.writeFile fn "" -- TODO: properly cleanup the file
  renderMarkupToByteStringIO 
    (B.appendFile fn) -- TODO: keep handle opened instead of reopening each time.
    markup
