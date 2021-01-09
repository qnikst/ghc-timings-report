{-# LANGUAGE StrictData #-}
-- | Definition of the types used in the analysis.
module GhcBuildPhase
  ( Phase(..)
  , parsePhases
  ) where

import Data.Aeson
import Data.Csv
import Data.Function
import Data.Maybe
import qualified Data.Text as T
import GHC.Generics
import TextShow
import TextShow.Generic

-- | Build phase that is reported in the ghc timings output.
data Phase = Phase
  { phaseName :: T.Text
  , phaseModule :: T.Text
  , phaseAlloc :: Int
  , phaseTime :: Double
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving anyclass (ToNamedRecord, DefaultOrdered)
  deriving TextShow via (FromGeneric Phase)

-- | Parse .ghc-timings file timings file and get list of phases.
--
-- This is ad-hoc parsing procedure that doesn't do anything clever like parsers-combinators
-- regular expressions and stuff.
--
-- Assumes struture:
-- @name [Module]: alloc=INT time=DOUBLE@
--
-- Doesn't report errors.
parsePhases :: T.Text -> [Phase]
parsePhases input = T.lines input & mapMaybe parseStep where
  parseStep x = case T.span (/='[') x of
    (phaseName, T.drop 1 -> rest1) -> case T.span (/=']') rest1 of
      (phaseModule, T.drop 2 -> rest2) -> case T.words rest2 of
        [allocs, time] ->
          let phaseAlloc = read $ T.unpack $ fromJust $  T.stripPrefix "alloc=" allocs -- !!!
              phaseTime = read $ T.unpack $ fromJust $  T.stripPrefix "time=" time -- !!!
          in Just Phase{..}
        _ -> Nothing -- error $ "illegal line: " <> T.unpack rest2
