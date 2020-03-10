-- | Html reports.
module Report
  ( index
  , packageTable
  , moduleTable
  ) where

import qualified Data.Text as T
import Data.Foldable (for_)
import Data.List (intercalate)
import Data.String
import Data.Text.Lazy.Builder.RealFloat
import GhcFile
import GhcBuildPhase
import Text.Blaze.Html5 hiding (map, head)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes hiding (title, rows, accept)
import qualified Text.Blaze.Html5.Attributes as A
import System.FilePath

index :: [String] -> Markup -- TODO: use T.Text
index packages = docTypeHtml $ do
  H.head $ do
    title "GHC timings report"
    H.link ! A.rel "stylesheet"
           ! A.type_ "text/css"
           ! A.href "./main.css"
  body ! A.style "width: 500px; margin: 0px auto 0px auto;" $ do
    h1 "Packages"
    p $ do
      "List of the packages in the project"
    ul $ for_ packages $ \package ->
         li
            $ a ! A.href (fromString $ "./" <> package <> ".html")
                $ toMarkup package

-- | Mark report page per package.
packageTable :: String -> [T.Text] -> [(GhcFile, Double, [Maybe Double])] -> Markup
packageTable package_name columns rows = docTypeHtml $ do
  H.head $ do
    title $ toMarkup $ "GHC timings report: " <> package_name
    H.link ! A.rel "stylesheet"
           ! A.type_ "text/css"
           ! A.href "./main.css"
  body $ do
    h1 $ fromString package_name
    a ! href "./index.html"
      $ "< back"
    table $ do
      tbody $ do
        tr $ do
          th "module name"
          th "total"
          for_ columns $ th . toMarkup
        for_ rows $ \(f@GhcFile{..}, total, cs) ->
          tr $ do 
            td ! A.class_ "module" $ 
               a ! href (fromString $ rebuildPlainPath f <> ".html")
                 $ toMarkup $ joinPath modulePath
            (td `lev` total ! A.class_ "number" ) $ toMarkup $ formatRealFloat Fixed (Just 2) total
            for_ cs $ \case 
              Nothing -> td mempty
              Just v -> (td `lev` v ! A.class_ "number") . toMarkup . formatRealFloat Fixed (Just 2) $ v
  where
    lev t v | v >= 10000 = t ! A.class_ "lev3"
            | v >= 5000 = t ! A.class_ "lev2"
            | v >= 1000 = t ! A.class_ "lev1"
            | otherwise = t
    
moduleTable :: GhcFile -> [Phase] -> Markup
moduleTable GhcFile{..} rows = docTypeHtml $ do
  H.head $ do
    title $ toMarkup $ "GHC timings report: " <> packageName <> ": " <> (intercalate "." modulePath)
    H.link ! A.rel "stylesheet"
           ! A.type_ "text/css"
           ! A.href "./main.css"
  body $ do
    h1 $ toMarkup $ packageName <> ": " <> (intercalate "." modulePath)
    p $ do
      a ! href "./index.html" $ "< index"
      a ! href (fromString $ "./" <> packageName <> ".html") $ toMarkup $ "< " <> packageName
    table $ do
      tbody $ do
        tr $ do
          th "Phase name"
          th "Alloc"
          th "Time"
        for_ rows $ \Phase{..} ->
          tr $ do 
            td ! A.class_ "module" $ toMarkup phaseName
            td ! A.class_ "number" $ toMarkup phaseAlloc
            (td `lev` phaseTime ! A.class_ "number") $ toMarkup $ formatRealFloat Fixed (Just 2) phaseTime
  where
    lev t v | v >= 10000 = t ! A.class_ "lev3"
            | v >= 5000 = t ! A.class_ "lev2"
            | v >= 1000 = t ! A.class_ "lev1"
            | otherwise = t
