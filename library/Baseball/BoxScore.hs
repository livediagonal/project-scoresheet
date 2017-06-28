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
import Baseball.Game.FrameState
import Baseball.Game.GameEvent
import Retrosheet.Events

data BoxScore
  = BoxScore
  { boxScoreHomeTeam :: TeamBoxScore
  , boxScoreAwayTeam :: TeamBoxScore
  }

data TeamBoxScore
  = TeamBoxScore
  { teamBoxScoreBatting :: Batting
  , teamBoxScorePitching :: Pitching
  }

makeClassy_ ''BoxScore
makeClassy_ ''TeamBoxScore

initialBoxScore :: BoxScore
initialBoxScore = BoxScore initialTeamBoxScore initialTeamBoxScore

initialTeamBoxScore :: TeamBoxScore
initialTeamBoxScore = TeamBoxScore initialBatting initialPitching

generateBoxScore :: Game -> BoxScore
generateBoxScore = foldl' (flip updateBoxScore) initialBoxScore . gameEvents

updateBoxScore :: GameEvent -> BoxScore -> BoxScore
updateBoxScore (GameEvent (StartEventType event) _ _) = 
  case startEventPlayerHome event of
    Away -> over _boxScoreAwayTeam (processStartEvent event)
    Home -> over _boxScoreHomeTeam (processStartEvent event)
updateBoxScore (GameEvent (SubEventType event) _ _) = 
  case subEventPlayerHome event of
    Away -> over _boxScoreAwayTeam (processSubEvent event)
    Home -> over _boxScoreHomeTeam (processSubEvent event)
updateBoxScore (GameEvent (PlayEventType event) _ fs) =
  case playEventHomeOrAway event of
    Away -> over _boxScoreAwayTeam (processPlayEvent event fs)
    Home -> over _boxScoreHomeTeam (processPlayEvent event fs)
updateBoxScore _ = id

processInfoEvent :: InfoEvent -> Game -> Game
processInfoEvent InfoEvent{..} = do
  let info = Just infoEventValue
  case infoEventKey of
    "visteam" -> _gameAwayTeam .~ info
    "hometeam" -> _gameHomeTeam .~ info
    "date" -> _gameDate .~ info
    "starttime" -> _gameStartTime .~ info
    _ -> id

processStartEvent :: StartEvent -> TeamBoxScore -> TeamBoxScore
processStartEvent StartEvent{..} =
  over _teamBoxScoreBatting (addPlayerToBatting startEventPlayer startEventBattingPosition) .
  over _teamBoxScorePitching (addPlayerToPitching startEventPlayer startEventFieldingPosition)

processSubEvent :: SubEvent -> TeamBoxScore -> TeamBoxScore
processSubEvent SubEvent{..} = 
    over _teamBoxScoreBatting (addPlayerToBatting subEventPlayer subEventBattingPosition) .
    over _teamBoxScorePitching (addPlayerToPitching subEventPlayer subEventFieldingPosition)

processPlayEvent :: PlayEvent -> FrameState -> TeamBoxScore -> TeamBoxScore
processPlayEvent event state = 
  over _teamBoxScoreBatting (updateBattingWithPlay event state) .
  over _teamBoxScorePitching (addPlayToPitching event state)

prettyPrintBoxScore :: BoxScore -> Text
prettyPrintBoxScore BoxScore{..} = 
  "Away\n" <> prettyPrintTeamBoxScore boxScoreAwayTeam <>
  "Home\n" <> prettyPrintTeamBoxScore boxScoreHomeTeam

prettyPrintTeamBoxScore :: TeamBoxScore -> Text
prettyPrintTeamBoxScore TeamBoxScore{..} = prettyPrintBatting teamBoxScoreBatting
  <> prettyPrintPitching teamBoxScorePitching