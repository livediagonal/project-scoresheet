{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module ProjectScoresheet.BoxScore.Pitching where

import ClassyPrelude
import Control.Lens
import qualified Data.HashMap.Strict as HashMap

import ProjectScoresheet.BaseballTypes
import ProjectScoresheet.Play

data Pitching
  = Pitching
  { pitchingHomeOrder :: ![PitchingLine]
  , pitchingAwayOrder :: ![PitchingLine]
  } deriving (Eq, Show)

data PitchingLine
  = PitchingLine
  { pitchingLinePlayer :: !Text
  , pitchingLineStrikes :: !Int
  , pitchingLineBalls :: !Int
  } deriving (Eq, Show, Generic)

makeClassy_ ''Pitching

initialPitching :: Pitching
initialPitching = Pitching [] []

initialPitchingLine :: Text -> PitchingLine
initialPitchingLine player = PitchingLine player 0 0

addPlayerToPitching :: HomeOrAway -> Text -> FieldingPosition -> Pitching -> Pitching
addPlayerToPitching Home player Pitcher = over _pitchingHomeOrder (initialPitchingLine player :)
addPlayerToPitching Away player Pitcher = over _pitchingAwayOrder (initialPitchingLine player :)
addPlayerToPitching _ _ _ = id

prettyPrintPitching :: Pitching -> Text
prettyPrintPitching Pitching{..} = unlines
  [ "-------------------------------------------"
  , "Home Pitchers   IP   H   R   ER   W  SO  HR"
  , "-------------------------------------------"
  , prettyPrintPitchingLines pitchingHomeOrder
  , "-------------------------------------------"
  , "Away Pitchers   IP   H   R   ER   W  SO  HR"
  , "-------------------------------------------"
  , prettyPrintPitchingLines pitchingAwayOrder
  ]

prettyPrintPitchingLines :: [PitchingLine] -> Text
prettyPrintPitchingLines pls = unlines $ map prettyPrintPitchingLine (reverse pls)

prettyPrintPitchingLine :: PitchingLine -> Text
prettyPrintPitchingLine PitchingLine{..} = pitchingLinePlayer
