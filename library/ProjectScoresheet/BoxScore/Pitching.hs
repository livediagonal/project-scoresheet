{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module ProjectScoresheet.BoxScore.Pitching 
  ( Pitching
  , initialPitching
  , addPlayerToPitching
  , addPlayToPitching
  , prettyPrintPitching
  ) where

import ClassyPrelude
import Control.Lens

import ProjectScoresheet.BaseballTypes
import ProjectScoresheet.Game.FrameState
import ProjectScoresheet.Play
import ProjectScoresheet.Retrosheet.Events

data Pitching = Pitching { pitchingLines :: ![PitchingLine] } deriving (Eq, Show)

data PitchingLine
  = PitchingLine
  { pitchingLinePlayer :: !Text
  , pitchingLineStrikes :: !Int
  , pitchingLineBalls :: !Int
  } deriving (Eq, Show, Generic)

makeClassy_ ''Pitching
makeClassy_ ''PitchingLine

initialPitching :: Pitching
initialPitching = Pitching []

initialPitchingLine :: Text -> PitchingLine
initialPitchingLine player = PitchingLine player 0 0

addPlayerToPitching :: Text -> FieldingPosition -> Pitching -> Pitching
addPlayerToPitching player Pitcher = over _pitchingLines (initialPitchingLine player :)
addPlayerToPitching _ _ = id

addPlayToPitching :: PlayEvent -> FrameState -> Pitching -> Pitching
addPlayToPitching _ _ = id

prettyPrintPitching :: Pitching -> Text
prettyPrintPitching Pitching{..} = unlines
  [ "-------------------------------------------"
  , "Pitchers        IP   H   R   ER   W  SO  HR"
  , "-------------------------------------------"
  , prettyPrintPitchingLines pitchingLines
  ]

prettyPrintPitchingLines :: [PitchingLine] -> Text
prettyPrintPitchingLines pls = unlines $ map prettyPrintPitchingLine (reverse pls)

prettyPrintPitchingLine :: PitchingLine -> Text
prettyPrintPitchingLine PitchingLine{..} = pitchingLinePlayer
