{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
module Compare where

import Control.Monad
import Data.Char (ord)
import Data.Csv as Csv
import Data.Csv.Builder as Csv
import Data.Function
import Data.List
import Data.Maybe as M
import Prelude hiding (mapM_, print)
import System.Directory
import System.FilePath
import System.Process


import qualified Data.ByteString.Lazy as BSL
import qualified Data.Binary.Builder as Builder
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as V (write) 
import qualified Prelude

import GhcBuildPhase

runCompare :: FilePath -> FilePath -> FilePath -> IO ()
runCompare beforeDir afterDir out = do
  -- ensure output directory created
  -- and templates copied into it
  createDirectoryIfMissing True out
  copyFile "files/module.template.gnuplot" (out </> "module.template.gnuplot")
  copyFile "files/package.template.gnuplot" (out </> "package.template.gnuplot")

  -- ensure both report directories exist
  checkDirectoriesExistence beforeDir afterDir

  -- read CSV file paths from both directories
  dirContents0 <- fixPaths beforeDir =<< (filterCSV <$> listDirectory beforeDir)
  dirContents1 <- fixPaths afterDir =<< filterCSV <$> listDirectory afterDir

  -- combine requried data from both reports
  -- and make a plot for all compilation stages for all modules
  -- and make one more report for a whole package/project
  modulesData0 <- getModulesDataFromReport dirContents0
  modulesData1 <- getModulesDataFromReport dirContents1
  let mergedModulesData = mergeModulesReports modulesData0 modulesData1
  withCurrentDirectory out $ do
    renderModules mergedModulesData
    renderPackage mergedModulesData
  
-- ** Helpers for `compare`

checkDirectoriesExistence :: FilePath -> FilePath -> IO ()
checkDirectoriesExistence before after = do
  exist0 <- doesDirectoryExist before
  exist1 <- doesDirectoryExist after
  let raiseErr x = error $ "Directory not exist: " <> x
  when (not exist0) $ raiseErr before
  when (not exist1) $ raiseErr after

filterCSV :: [FilePath] -> [FilePath]
filterCSV = Prelude.filter ((== ".dump-timings.csv") . takeExtensions)

fixPaths :: FilePath -> [FilePath] -> IO [FilePath]
fixPaths dir = Prelude.mapM (makeAbsolute . (dir </>))

getPackageNameFromReport :: [FilePath] -> FilePath
getPackageNameFromReport xs
  = maybe (error "Unable to get package name from a directory") id
  $ listToMaybe $ sortBy (compare `on` Prelude.length) xs

dropPackageNameFromReport :: [FilePath] -> [FilePath]
dropPackageNameFromReport xs
  = Prelude.tail $ sortBy (compare `on` Prelude.length) xs

getModulesDataFromReport :: [FilePath] -> IO [(T.Text, V.Vector Phase)]
getModulesDataFromReport xs = do
  modulesData <- forM xs parseFile >>= (pure . M.catMaybes)
  pure $ sortBy (compare `on` fst) modulesData
  where
    parseFile file = do
      contents <- BSL.readFile file
      case decodeByName contents of
        Left err -> do
          putStrLn $ "getModulesDataFromReport : " <> show err
          pure Nothing
        Right (_headers, result) -> if V.null result
          then pure Nothing
          else pure $ Just $ (phaseModule $ V.head result, result)

mergeModulesReports
  :: [(T.Text, V.Vector Phase)] -> [(T.Text, V.Vector Phase)] -> V.Vector ModuleReportRow
mergeModulesReports modulesBefore modulesAfter =
  let -- helpers
      planModules
        :: [(T.Text, V.Vector Phase)] -> [(T.Text, V.Vector Phase)] -> Map.Map T.Text Int
      planModules before after = snd $ go (0, Map.empty) (fst <$> before) (fst <$> after)
        where
          go xs [] [] = xs
          go (ix, xs) [] as
            = (ix, List.foldl' insertIfNotExist xs $ zip as [ix ..])
          go (ix, xs) bs []
            = (ix, List.foldl' insertIfNotExist xs $ zip bs [ix ..])
          go (ix, xs) oldB@(b : bs) oldA@(a : as) = case b `compare` a of
            EQ -> go (ix + 1, insertIfNotExist xs (a, ix)) bs as
            LT -> go (ix + 1, insertIfNotExist xs (b, ix)) bs oldA
            GT -> go (ix + 1, insertIfNotExist xs (a, ix)) oldB as
          insertIfNotExist m (k, v) = Map.alter (f v) k m
            where
              f v' Nothing = Just v'
              f _ (Just oldV) = Just oldV

      gatherModuleDiffs
        :: Map.Map T.Text Int
        -> [(T.Text, V.Vector Phase)]
        -> [(T.Text, V.Vector Phase)]
        -> V.Vector ModuleDiff
      gatherModuleDiffs moduleMap before after = V.catMaybes
        $ go (V.replicate (Prelude.succ $ Prelude.maximum $ (-1) : Map.elems moduleMap) Nothing) before after
        where
          go
            :: V.Vector (Maybe ModuleDiff)
            -> [(T.Text, V.Vector Phase)]
            -> [(T.Text, V.Vector Phase)]
            -> V.Vector (Maybe ModuleDiff)
          go v [] [] = v
          go vec bs []
            = let update
                    :: V.Vector (Maybe ModuleDiff)
                    -> (T.Text, V.Vector Phase)
                    -> V.Vector (Maybe ModuleDiff)
                  update v (moduleName, moduleData)
                    = V.modify (modifier makeBefore moduleName moduleData) v
                  modifier make name data' v
                    = V.write v ix
                    $ Just $ ModuleData
                        { moduleDataIndex = ix
                        , moduleDataModuleName = name
                        , moduleDataData = make data'
                        }
                    where
                      ix = moduleMap Map.! name
                        
              in List.foldl' update vec bs
          go vec [] as
            = let update v (moduleName, moduleData)
                    = V.modify (modifier makeAfter moduleName moduleData) v
                  modifier make name data' v
                    = V.write v ix
                    $ Just $ ModuleData
                    { moduleDataIndex = ix
                    , moduleDataModuleName = name
                    , moduleDataData = make data'
                    }
                    where
                      ix = moduleMap Map.! name
              in List.foldl' update vec as
          go vec oldB@(b : bs) oldA@(a : as) = case fst b `compare` fst a of
            EQ -> let update v moduleName moduleDataBefore moduleDataAfter 
                        = V.modify
                          (modifier moduleName moduleDataBefore moduleDataAfter)
                          v
                      modifier name dataB dataA v
                        = V.write v ix
                        $ Just $ ModuleData
                            { moduleDataIndex = ix
                            , moduleDataModuleName = name
                            , moduleDataData = makeBoth dataB dataA
                            }
                        where
                          ix = moduleMap Map.! name
                  in go (update vec (fst b) (snd b) (snd a)) bs as
            LT -> let update v make (moduleName, moduleData) 
                        = V.modify (modifier make moduleName moduleData) v
                      modifier make name data' v
                        = V.write v ix
                        $ Just $ ModuleData
                            { moduleDataIndex = ix
                            , moduleDataModuleName = name
                            , moduleDataData = make data'
                            }
                        where
                          ix = moduleMap Map.! name
                  in go (update vec makeBefore b) bs oldA
            GT -> let update v make (moduleName, moduleData) 
                        = V.modify (modifier make moduleName moduleData) v
                      modifier make name data' v
                        = V.write v ix
                        $ Just $ ModuleData
                            { moduleDataIndex = ix
                            , moduleDataModuleName = name
                            , moduleDataData = make data'
                            }
                        where
                          ix = moduleMap Map.! name
                  in go (update vec makeAfter a) oldB as

      makeModuleReport :: ModuleDiff -> ModuleReportRow
      makeModuleReport old = ModuleData
        { moduleDataIndex = moduleDataIndex old
        , moduleDataModuleName = moduleDataModuleName old
        , moduleDataData = transform (moduleDataData old)
        }
        where
          transform :: Diff (V.Vector Phase) -> V.Vector PhaseReport
          transform Diff{..} = goTransform before after

          goTransform Nothing Nothing = V.empty
          goTransform (Just b) Nothing = V.map (makePhaseReportWith makeBefore) (V.indexed b)
          goTransform Nothing (Just a) = V.map (makePhaseReportWith makeAfter) (V.indexed a)
          goTransform (Just b) (Just a) = V.fromList $ List.reverse
            $ buildWholeSequence (V.toList b) (V.toList a)

          makePhaseReportWith make (ix, Phase{..}) = PhaseReport
            { phaseReportPhaseName = phaseName
            , phaseReportIndex     = ix
            , phaseDiffTime        = make phaseTime
            }

          buildWholeSequence bs as
            = runBuilder bs as $ optimize $ List.reverse (go [] bs as)
            where
              go result [] [] = result
              go result [] ys = TakeSecond Forward (List.length ys) : result
              go result xs [] = TakeFirst Forward (List.length xs) : result
              go result (x : xs) (y : ys) =
                if phaseName x == phaseName y
                  then go (TakeBoth Forward 1 : result) xs ys
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
              goDeeper result [] ys = go result [] ys
              goDeeper result xs [] = go result xs []
              goDeeper result xs ys = if List.length xs <= List.length ys
                then let SubsequenceData{..} =
                           goFindSubsequences (emptySubsequenceData @Phase) xs ys
                     in go (TakeBoth Forward commonSubsequenceLength : TakeFirst Forward prefixLength : result) restOfFirst restOfSecond
                else let SubsequenceData{..} = goFindSubsequences emptySubsequenceData ys xs
                     in go (TakeBoth Forward commonSubsequenceLength : TakeSecond Forward prefixLength : result) restOfSecond restOfFirst

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

      -- data
      moduleIndices :: Map.Map T.Text Int
      moduleIndices = planModules modulesBefore modulesAfter

      moduleDiffs :: V.Vector ModuleDiff
      moduleDiffs = gatherModuleDiffs moduleIndices modulesBefore modulesAfter

      moduleReport :: V.Vector ModuleReportRow
      moduleReport = V.map makeModuleReport moduleDiffs

  in moduleReport

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


data Diff a = Diff { before :: Maybe a, after :: Maybe a }
  deriving (Eq, Show)

makeAfter, makeBefore :: a -> Diff a
makeBefore a = Diff { before = Just a, after = Nothing }
makeAfter a = Diff { before = Nothing, after = Just a }

makeBoth :: a -> a -> Diff a
makeBoth b a = Diff { before = Just b, after = Just a }

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
  removeFile "module.template.gnuplot"
  where
    opts = Csv.defaultEncodeOptions { Csv.encDelimiter = fromIntegral (ord '\t') }
    renderRow row = 
      let content = mconcat
            (Prelude.map (Csv.encodeRecordWith opts)
             [ ( phaseReportPhaseName pr
               , moduleDataModuleName row
               , fromMaybe 0.0 $ before $ phaseDiffTime pr
               , fromMaybe 0.0 $ after $ phaseDiffTime pr
               )
             | pr <- V.toList (moduleDataData row)
             ])
      in (moduleDataModuleName row, content)
    buildPlot (moduleName, bs) = do
      plotSettingsTemplate <- T.readFile "module.template.gnuplot"
      let plotSettingsFile = T.unpack moduleName <.> "gnuplot"
          outputFile = T.unpack moduleName <.> "svg"
          plotDataFile = T.unpack moduleName <.> "dat"
          plotSettingsContents
            = T.replace "<INPUT_FILE>" (T.pack plotDataFile)
            $ T.replace "<TITLE>" moduleName
            $ T.replace "<OUTPUT_FILE>" (T.pack outputFile)
              plotSettingsTemplate
      T.writeFile plotSettingsFile plotSettingsContents
      BSL.writeFile plotDataFile $ Builder.toLazyByteString bs
      createPlot plotSettingsFile
      removeFile plotSettingsFile
    createPlot plotFile = callProcess "gnuplot" [plotFile]

renderPackage rows = do
  let output = renderRow <$> V.toList rows
  buildPlot "package.svg" output
  mapM_ removeFile [ "package.template.gnuplot", "package.gnuplot" ]
  where
    opts = Csv.defaultEncodeOptions { Csv.encDelimiter = fromIntegral (ord '\t') }
    renderRow row = Csv.encodeRecordWith opts
      (moduleDataModuleName row, totalBefore row, totalAfter row)
    total what = Prelude.sum . (0 :) . V.toList
      . V.catMaybes . V.map (what . phaseDiffTime) . moduleDataData
    totalBefore = total before
    totalAfter = total after
    buildPlot outputFile bs = do
      plotSettingsTemplate <- T.readFile "package.template.gnuplot"
      let plotDataFile = "package.dat"
          plotSettingsFile = "package.gnuplot"
          plotSettingsContents
            = T.replace "<INPUT_FILE>" (T.pack plotDataFile)
            $ T.replace "<OUTPUT_FILE>" outputFile
              plotSettingsTemplate
      T.writeFile plotSettingsFile plotSettingsContents
      BSL.writeFile plotDataFile $ Builder.toLazyByteString $ mconcat bs
      createPlot plotSettingsFile
    createPlot plotFile = callProcess "gnuplot" [plotFile]
