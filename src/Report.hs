-- | Html reports.
module Report
  ( index
  , packageTable
  , moduleTable
  ) where

import Colonnade
import qualified Data.Text as T
import Data.Foldable (for_)
import Data.List (intercalate)
import Data.String
import Data.Text.Lazy.Builder.RealFloat
import GhcFile
import GhcBuildPhase
import Text.Blaze.Colonnade
import Text.Blaze.Html5 hiding (map, head)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes hiding (title, rows, accept)
import qualified Text.Blaze.Html5.Attributes as A
import System.FilePath
import TextShow (showt)
import Prelude hiding (span)

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
packageTable package_name columns' rows = docTypeHtml $ do
  H.head $ do
    title $ toMarkup $ "GHC timings report: " <> package_name
    H.link ! A.rel "stylesheet"
           ! A.type_ "text/css"
           ! A.href "./main.css"
  body $ do
    h1 $ fromString package_name
    p $ do
      a ! href "./index.html" $ "index"
    p $ do
      a ! href (fromString $ package_name <> ".csv")
        $ "Download as CSV"
    encodeCellTable
      mempty
      (headed ((textCell "module name"){ cellAttribute = A.scope "col"})
              (\(f@GhcFile{..},_,_) -> (htmlCell
                  $ a ! href (fromString $ rebuildPlainPath f <> ".html")
                  $ toMarkup $ joinPath modulePath)
                  { cellAttribute = A.class_ "module"}
              )
       <> headed ((textCell "total"){ cellAttribute = A.scope "col"})
           (\(_,total,_) -> (htmlCell $ toMarkup $ formatRealFloat Fixed (Just 2) total)
              { cellAttribute = lev total <> A.class_ "number"} )
       <> mconcat [ headed ((textCell c){ cellAttribute = A.scope "col"})
                     (\(_,_,z) -> (htmlCell $ maybe mempty (toMarkup . formatRealFloat Fixed (Just 2)) $ z!!j)
                        { cellAttribute = (maybe mempty lev) (z!!j) <> A.class_ "number"} )
                  | (j,c) <- zip [0..] columns'])
      rows
  where
    lev v | v >= 10000 = A.class_ "lev3"
          | v >= 5000 = A.class_ "lev2"
          | v >= 1000 = A.class_ "lev1"
          | otherwise = mempty

moduleTable :: GhcFile -> [Phase] -> Markup
moduleTable f@GhcFile{..} rows = docTypeHtml $ do
  H.head $ do
    title $ toMarkup $ "GHC timings report: " <> packageName <> ": " <> (intercalate "." modulePath)
    H.link ! A.rel "stylesheet"
           ! A.type_ "text/css"
           ! A.href "./main.css"
  body $ do
    h1 $ toMarkup $ packageName <> ": " <> (intercalate "." modulePath)
    p $ do
      a ! href "./index.html" $ "index"
      H.span " < "
      a ! href (fromString $ "./" <> packageName <> ".html") $ toMarkup packageName
    p $ do
      a ! href (fromString $ rebuildPlainPath f <> ".csv")
        $ "Download as CSV"
    encodeCellTable
      mempty
      (headed (textCell "number")
              (\(n,_) -> textCell (showt n))
       <> headed (textCell "Phase name")
                (\(_,Phase{..}) -> textCell phaseName)
       <> headed ((textCell "Alloc"))
           (\(_,Phase{..}) -> textCell $ showt phaseAlloc)
       <> headed (textCell "Time")
                 (\(_,Phase{..}) ->
                   (htmlCell $ toMarkup $ formatRealFloat Fixed (Just 2) phaseTime)
                        { cellAttribute = lev phaseTime <> A.class_ "number"} ))
       $ zip [(1::Int)..] rows
  where
    lev v | v >= 10000 = A.class_ "lev3"
          | v >= 5000 = A.class_ "lev2"
          | v >= 1000 = A.class_ "lev1"
          | otherwise = mempty
