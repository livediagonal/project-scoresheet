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
import Retrosheet.Events

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
addEventToBoxScore (GameEvent (StartEventType event) _ _) =
  case startEventPlayerHome event of
    Away -> over _boxScoreAwayTeam (processStartEvent event)
    Home -> over _boxScoreHomeTeam (processStartEvent event)
addEventToBoxScore (GameEvent (SubEventType event) _ _) =
  case subEventPlayerHome event of
    Away -> over _boxScoreAwayTeam (processSubEvent event)
    Home -> over _boxScoreHomeTeam (processSubEvent event)
addEventToBoxScore (GameEvent (PlayEventType event) gs fs) =
  case playEventHomeOrAway event of
    Away ->
      over _boxScoreAwayTeam (over _teamStatisticsBatting (addPlayToBatting event fs)) .
      over _boxScoreHomeTeam (over _teamStatisticsPitching (addPlayToPitching event gs))
    Home ->
      over _boxScoreHomeTeam (over _teamStatisticsBatting (addPlayToBatting event fs)) .
      over _boxScoreAwayTeam (over _teamStatisticsPitching (addPlayToPitching event gs))
addEventToBoxScore _ = id

processStartEvent :: StartEvent -> TeamStatistics -> TeamStatistics
processStartEvent StartEvent{..} =
  over _teamStatisticsBatting (addPlayerToBatting startEventPlayer startEventBattingPosition) .
  over _teamStatisticsPitching (addPlayerToPitching startEventPlayer startEventFieldingPosition)

processSubEvent :: SubEvent -> TeamStatistics -> TeamStatistics
processSubEvent SubEvent{..} =
    over _teamStatisticsBatting (addPlayerToBatting subEventPlayer subEventBattingPosition) .
    over _teamStatisticsPitching (addPlayerToPitching subEventPlayer subEventFieldingPosition)

prettyPrintBoxScore :: BoxScore -> Text
prettyPrintBoxScore BoxScore{..} =
  "Away\n" <> prettyPrintTeamStatistics boxScoreAwayTeam <>
  "Home\n" <> prettyPrintTeamStatistics boxScoreHomeTeam

prettyPrintTeamStatistics :: TeamStatistics -> Text
prettyPrintTeamStatistics TeamStatistics{..} = prettyPrintBatting teamStatisticsBatting
  <> prettyPrintPitching teamStatisticsPitching
