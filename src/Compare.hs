{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
module Compare
  ( runCompare
  ) where

import Control.Exception
import Control.Monad
import Data.Char (ord)
import Data.Csv as Csv
import Data.Csv.Builder as Csv
import Data.Foldable
import Data.Function
import Data.Functor
import Data.List
import Data.Maybe as M
import Data.These
import Data.These.Combinators
import Data.Traversable
import Prelude hiding (mapM_, print)
import System.Directory
import System.Environment
import System.FilePath
import System.Process
import System.IO.Unsafe


import qualified Data.Binary.Builder as Builder
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as List
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import qualified Prelude

import GhcBuildPhase
import Paths_ghc_timings (getDataFileName)

pattern MODULE_TEMPLATE_FILENAME :: FilePath
pattern MODULE_TEMPLATE_FILENAME = "module.template.gnuplot"

pattern PACKAGE_TEMPLATE_FILENAME :: FilePath
pattern PACKAGE_TEMPLATE_FILENAME = "package.template.gnuplot"

pattern DUMP_TIMINGS_PATTERN :: FilePath
pattern DUMP_TIMINGS_PATTERN = ".dump-timings.csv"

pattern INPUT_FILE, OUTPUT_FILE, TITLE :: T.Text
pattern INPUT_FILE = "<INPUT_FILE>"
pattern OUTPUT_FILE = "<OUTPUT_FILE>"
pattern TITLE = "<TITLE>"

pattern GNUPLOT_CMD :: String
pattern GNUPLOT_CMD = "gnuplot"

pattern DEBUG_ENV :: String
pattern DEBUG_ENV = "DEBUG"

-- | A helper function that allows instead of writing comments emit an output.
--
-- Actually it's just a less generic Debug.Trace.traceM.
explain :: String -> IO ()
{-# NOINLINE explain #-}
explain = unsafePerformIO $ do
  lookupEnv DEBUG_ENV >>= \case
    Nothing -> pure \_ -> pure ()
    Just{}  -> pure \s -> putStrLn $ "Debug: " <> s

runCompare :: FilePath -> FilePath -> FilePath -> IO ()
runCompare beforeDir afterDir out = do
  explain "ensure output directory created"
  createDirectoryIfMissing True out
  explain "ensure both report directories exist"
  checkDirectoriesExistence beforeDir afterDir
  explain "read CSV file paths from both directories"
  explain "combine requried data from both reports"
  mergedModulesData <- prepareData
  explain "and make a plot for all compilation stages for all modules"
  explain "and make one more report for a whole package/project"
  visualizeData mergedModulesData
  where
    prepareData :: IO (V.Vector ModuleReportRow)
    prepareData = do
      dirContentsBefore <- fixPaths beforeDir =<< filterCSV <$> listDirectory beforeDir
      dirContentsAfter <- fixPaths afterDir =<< filterCSV <$> listDirectory afterDir
      modulesDataBefore <- getModulesDataFromReport dirContentsBefore
      modulesDataAfter <- getModulesDataFromReport dirContentsAfter
      let mergedModulesData = mergeModulesReports modulesDataBefore modulesDataAfter
      pure mergedModulesData
    visualizeData :: V.Vector ModuleReportRow -> IO ()
    visualizeData mergedModulesData =
      withCurrentDirectory out $
        withMaterializedFile MODULE_TEMPLATE_FILENAME $
          withMaterializedFile PACKAGE_TEMPLATE_FILENAME $ do
            renderModules mergedModulesData
            renderPackage mergedModulesData


-- ** Helpers for `compare`

-- | Check if required directores does exist.
checkDirectoriesExistence :: FilePath -> FilePath -> IO ()
checkDirectoriesExistence before after = traverse_ check [before, after] where
  check x = do
    exists <- doesDirectoryExist x
    when (not exists) $ error $ "Directory not exist: " <> x

-- | Filters .dump-timings files.
filterCSV :: [FilePath] -> [FilePath]
filterCSV = Prelude.filter ((== DUMP_TIMINGS_PATTERN) . takeExtensions)

-- | Makes path an absolute one
fixPaths :: FilePath -> [FilePath] -> IO [FilePath]
fixPaths dir = traverse (makeAbsolute . (dir </>))

getModulesDataFromReport :: [FilePath] -> IO (Map.Map T.Text (V.Vector Phase))
getModulesDataFromReport xs = Map.fromList . M.catMaybes <$> for xs \file ->
  BSL.readFile file <&> decodeByName >>= \case
    Left err -> do
      putStrLn $ "getModulesDataFromReport : " <> show err
      pure Nothing
    Right (_headers, result)
      | V.null result -> pure Nothing
      | otherwise -> pure $ Just (phaseModule $ V.head result, result)

mergeModulesReports
  :: Map.Map (T.Text) (V.Vector Phase) -- ^ An old report
  -> Map.Map (T.Text) (V.Vector Phase) -- ^ A new port
  -> V.Vector ModuleReportRow
mergeModulesReports modulesBefore modulesAfter = moduleReport where
      moduleReport :: V.Vector ModuleReportRow
      moduleReport = V.map makeModuleReport moduleDiffs
      moduleDiffs :: V.Vector ModuleDiff
      moduleDiffs = gatherModuleDiffs modulesBefore modulesAfter


-- | Gather info for building a diff.
--
gatherModuleDiffs
  :: Map.Map T.Text (V.Vector Phase)
  -> Map.Map T.Text (V.Vector Phase)
  -> V.Vector ModuleDiff
gatherModuleDiffs before after = V.fromList
    $ zipWith
       (\moduleDataIndex (moduleDataModuleName, moduleDataData) -> ModuleData{..})
       [0..]
       (Map.toList moduleMap)
  where
    moduleMap = Map.merge 
       (Map.mapMissing $ \_key b -> makeBefore b) -- exists only in old datum
       (Map.mapMissing $ \_key a -> makeAfter a)  -- exists only in new datum
       (Map.zipWithMatched $ \_key a b -> makeBoth b a) -- exists in both data
       before
       after

makeModuleReport :: ModuleDiff -> ModuleReportRow
makeModuleReport ModuleData{..} = ModuleData
  { moduleDataData = transform moduleDataData
  , ..
  }
  where
    transform :: Diff (V.Vector Phase) -> V.Vector PhaseReport
    transform = \case
      This b -> V.map (makePhaseReportWith makeBefore) (V.indexed b)
      That a -> V.map (makePhaseReportWith makeAfter) (V.indexed a)
      These b a -> V.fromList $ List.reverse
        $ buildWholeSequence (V.toList b) (V.toList a)

    makePhaseReportWith make (ix, Phase{..}) = PhaseReport
      { phaseReportPhaseName = phaseName
      , phaseReportIndex     = ix
      , phaseDiffTime        = make phaseTime
      }

    buildWholeSequence bs as
      = runBuilder bs as $ optimize $ List.reverse (goFwd [] bs as)
      where
        goFwd result [] [] = result
        goFwd result [] ys = TakeSecond Forward (List.length ys) : result
        goFwd result xs [] = TakeFirst Forward (List.length xs) : result
        goFwd result (x : xs) (y : ys) =
          if phaseName x == phaseName y
            then goFwd (TakeBoth Forward 1 : result) xs ys
            else goBack result (xs, ys) (List.reverse xs) (List.reverse ys)

        goBack result _prev [] [] = result
        goBack result _prev [] ys = TakeSecond Backward (List.length ys) : result
        goBack result _prev xs [] = TakeFirst Forward (List.length xs) : result
        goBack result prev@(oldX, oldY) (x : xs) (y : ys) =
          if phaseName x == phaseName y
            then goBack (TakeBoth Backward 1 : result) (initBothEnds prev) xs ys
            else goDeeper result oldX oldY

        initBothEnds ([], []) = ([], [])
        initBothEnds ([], ys) = ([], List.init ys)
        initBothEnds (xs, []) = (List.init xs, [])
        initBothEnds (xs, ys) = (List.init xs, List.init ys)

        goDeeper result [] [] = result
        goDeeper result [] ys = goFwd result [] ys
        goDeeper result xs [] = goFwd result xs []
        goDeeper result xs ys = if List.length xs <= List.length ys
          then let SubsequenceData{..} =
                     goFindSubsequences (emptySubsequenceData @Phase) xs ys
               in goFwd (TakeBoth Forward commonSubsequenceLength : TakeFirst Forward prefixLength : result) restOfFirst restOfSecond
          else let SubsequenceData{..} = goFindSubsequences emptySubsequenceData ys xs
               in goFwd (TakeBoth Forward commonSubsequenceLength : TakeSecond Forward prefixLength : result) restOfSecond restOfFirst

        goFindSubsequences prev [] ys = prev { restOfSecond = ys }
        goFindSubsequences prev xs [] = prev { restOfFirst = xs }
        goFindSubsequences prev xs ys =
          let msubsequence = listToMaybe
                $ List.filter ((== (fmap phaseName xs)) . fmap phaseName . snd)
                $ subsequencesWithLength (List.length xs) ys
              new = prev { prefixLength = prefixLength prev + 1 }
          in case msubsequence of
            Nothing -> goFindSubsequences new xs ys
            Just (ix, subseq) ->
              prev { prefixLength = prefixLength prev + 1
                   , commonSubsequenceLength = List.length subseq
                   , restOfFirst = []
                   , restOfSecond = List.drop (List.length subseq + ix) ys
                   }
        subsequencesWithLength :: Int -> [a] -> [(Int, [a])]
        subsequencesWithLength l = snd . f (0, [])
          where
            f res [] = res
            f (ix, prev) listElems
              = f (ix + 1, (ix, List.take l listElems) : prev) (List.drop 1 listElems)

    optimize = id -- TODO: improve

    runBuilder xs' ys' actions = go [] (withIndices xs') (withIndices ys') actions
      where
        withIndices = zip [(0 :: Int) ..]
        go res _ _ [] = res
        go res xs ys (listAction : listActions) =
          let (newXs, newYs, newRes) = case listAction of
                TakeFirst Forward l ->
                  (List.drop l xs, ys, (mkBefore <$> List.take l xs) <> res)
                TakeSecond Forward l ->
                  (xs, List.drop l ys, (mkAfter <$> List.take l ys) <> res)
                TakeBoth Forward l ->
                  (List.drop l xs, List.drop l ys, (mkBoth <$> List.zip (List.take l xs) (List.take l ys)) <> res)
                TakeFirst Backward l ->
                  (dropEnd l xs, ys, res <> (mkBefore <$> takeEnd l xs))
                TakeSecond Backward l ->
                  (xs, dropEnd l ys, res <> (mkAfter <$> takeEnd l ys))
                TakeBoth Backward l ->
                  (dropEnd l xs, dropEnd l ys, res <> (mkBoth <$> List.zip (takeEnd l xs) (takeEnd l ys)))
              mkBefore (ix, Phase{..}) = PhaseReport
                { phaseReportPhaseName = phaseName
                , phaseReportIndex = ix
                , phaseDiffTime = makeBefore phaseTime
                }
              mkAfter (ix, Phase{..}) = PhaseReport
                { phaseReportPhaseName = phaseName
                , phaseReportIndex = ix
                , phaseDiffTime = makeAfter phaseTime
                }
              mkBoth ((ixb, b), (ixa, a)) = PhaseReport
                { phaseReportPhaseName = phaseName b
                , phaseReportIndex = min ixb ixa
                , phaseDiffTime = makeBoth (phaseTime b) (phaseTime a)
                }
              dropEnd l = List.reverse . List.drop l . List.reverse
              takeEnd l = List.reverse . List.take l . List.reverse
              in go newRes newXs newYs listActions

data PhaseReport = PhaseReport
  { phaseReportPhaseName :: T.Text
  , phaseReportIndex :: Int
  , phaseDiffTime :: Diff Double
  } deriving Show

data ModuleData a = ModuleData
  { moduleDataIndex :: Int
  , moduleDataModuleName :: T.Text
  , moduleDataData :: a
  } deriving Show


type Diff a = These a a

makeAfter, makeBefore :: a -> Diff a
makeBefore a = This a
makeAfter a = That a

makeBoth :: a -> a -> Diff a
makeBoth b a = These b a

type ModuleDiff = ModuleData (Diff (V.Vector Phase))

type ModuleReportRow = ModuleData (V.Vector PhaseReport)

-- ** Parser Helpers

data Direction = Forward | Backward
  deriving (Eq, Show, Ord)

data SequenceAction
  = TakeFirst Direction Int
  | TakeSecond Direction Int
  | TakeBoth Direction Int
  deriving (Eq, Show, Ord)

data SubsequenceData a = SubsequenceData
  { prefixLength :: Int
  , commonSubsequenceLength :: Int
  , restOfFirst :: [a]
  , restOfSecond :: [a]
  }

emptySubsequenceData :: forall a. SubsequenceData a
emptySubsequenceData = SubsequenceData
  { prefixLength = 0
  , commonSubsequenceLength = 0
  , restOfFirst = []
  , restOfSecond = []
  }

renderModules, renderPackage :: V.Vector ModuleReportRow -> IO ()
renderModules rows = do
  let output = renderRow <$> V.toList rows
  Prelude.mapM_ buildPlot output
  where
    opts = Csv.defaultEncodeOptions { Csv.encDelimiter = fromIntegral (ord '\t') }
    renderRow row =
      let content = mconcat
            (Prelude.map (Csv.encodeRecordWith opts)
             [ ( phaseReportPhaseName pr
               , moduleDataModuleName row
               , fromMaybe 0.0 $ justHere $ phaseDiffTime pr
               , fromMaybe 0.0 $ justThere $ phaseDiffTime pr
               )
             | pr <- V.toList (moduleDataData row)
             ])
      in (moduleDataModuleName row, content)
    buildPlot (moduleName, bs) = do
      plotSettingsTemplate <- T.readFile MODULE_TEMPLATE_FILENAME
      let plotSettingsFile = T.unpack moduleName <.> "gnuplot"
          outputFile = T.unpack moduleName <.> "svg"
          plotDataFile = T.unpack moduleName <.> "dat"
          plotSettingsContents
            = T.replace INPUT_FILE (T.pack plotDataFile)
            $ T.replace TITLE moduleName
            $ T.replace OUTPUT_FILE (T.pack outputFile)
              plotSettingsTemplate
      T.writeFile plotSettingsFile plotSettingsContents
      BSL.writeFile plotDataFile $ Builder.toLazyByteString bs
      createPlot plotSettingsFile
      removeFile plotSettingsFile
    createPlot plotFile = callProcess GNUPLOT_CMD [plotFile]

renderPackage rows = do
  let output = renderRow <$> V.toList rows
  buildPlot "package.svg" output
  where
    opts = Csv.defaultEncodeOptions { Csv.encDelimiter = fromIntegral (ord '\t') }
    renderRow row = Csv.encodeRecordWith opts
      (moduleDataModuleName row, totalBefore row, totalAfter row)
    totalBefore row = sum $ catThere $ fmap phaseDiffTime . V.toList $ moduleDataData $ row
    totalAfter row = sum $ catThere $ fmap phaseDiffTime . V.toList $ moduleDataData $ row
    buildPlot outputFile bs = do
      plotSettingsTemplate <- T.readFile PACKAGE_TEMPLATE_FILENAME
      let plotDataFile = "package.dat"
          plotSettingsFile = "package.gnuplot"
          plotSettingsContents
            = T.replace INPUT_FILE (T.pack plotDataFile)
            $ T.replace OUTPUT_FILE outputFile
              plotSettingsTemplate
      T.writeFile plotSettingsFile plotSettingsContents
      BSL.writeFile plotDataFile $ Builder.toLazyByteString $ mconcat bs
      createPlot plotSettingsFile
    createPlot plotFile = callProcess GNUPLOT_CMD [plotFile]

-- | Temporary materialize a file in the given directory. File is
-- removed when the function exits.
withMaterializedFile
  :: FilePath -- ^ File name
  -> IO a
  -> IO a
withMaterializedFile name f =
  getDataFileName name >>= \fp ->
    bracket_ (copyFile fp name) (removeFile name) f

