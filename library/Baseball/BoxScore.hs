{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Baseball.BoxScore where

import ClassyPrelude
import Control.Lens

import Baseball.BaseballTypes
import Baseball.BoxScore.Batting
import Baseball.BoxScore.Pitching
import Baseball.Game
import Baseball.Game.GameEvent
import Baseball.Game.GameState
import Baseball.Event

data BoxScore
  = BoxScore
  { boxScoreHomeTeam :: TeamStatistics
  , boxScoreAwayTeam :: TeamStatistics
  }

data TeamStatistics
  = TeamStatistics
  { teamStatisticsBatting :: Batting
  , teamStatisticsPitching :: Pitching
  }

makeClassy_ ''BoxScore
makeClassy_ ''TeamStatistics

initialBoxScore :: BoxScore
initialBoxScore = BoxScore initialTeamStatistics initialTeamStatistics

initialTeamStatistics :: TeamStatistics
initialTeamStatistics = TeamStatistics initialBatting initialPitching

generateBoxScore :: Game -> BoxScore
generateBoxScore = foldl' (flip addEventToBoxScore) initialBoxScore . gameEvents

addEventToBoxScore :: GameEvent -> BoxScore -> BoxScore
addEventToBoxScore (GameEvent (SubstitutionEvent event) _ _) =
  case subTeam event of
    Away -> over _boxScoreAwayTeam (processSubstitution event)
    Home -> over _boxScoreHomeTeam (processSubstitution event)
addEventToBoxScore (GameEvent (PlayEvent event) gs fs) =
  case currentTeam gs of
    Away ->
      over _boxScoreAwayTeam (over _teamStatisticsBatting (addPlayToBatting event fs)) .
      over _boxScoreHomeTeam (over _teamStatisticsPitching (addPlayToPitching event gs fs))
    Home ->
      over _boxScoreHomeTeam (over _teamStatisticsBatting (addPlayToBatting event fs)) .
      over _boxScoreAwayTeam (over _teamStatisticsPitching (addPlayToPitching event gs fs))

processSubstitution :: Substitution -> TeamStatistics -> TeamStatistics
processSubstitution Substitution{..} =
  over _teamStatisticsBatting (addPlayerToBatting subPlayer subBattingPosition) .
  over _teamStatisticsPitching (addPlayerToPitching subPlayer subFieldingPosition)

prettyPrintBoxScore :: BoxScore -> Text
prettyPrintBoxScore BoxScore{..} =
  "Away\n" <> prettyPrintTeamStatistics boxScoreAwayTeam <>
  "Home\n" <> prettyPrintTeamStatistics boxScoreHomeTeam

prettyPrintTeamStatistics :: TeamStatistics -> Text
prettyPrintTeamStatistics TeamStatistics{..} = prettyPrintBatting teamStatisticsBatting
  <> prettyPrintPitching teamStatisticsPitching
