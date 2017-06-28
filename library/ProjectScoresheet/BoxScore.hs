{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module ProjectScoresheet.BoxScore where

import ClassyPrelude
import Control.Lens

import ProjectScoresheet.BaseballTypes
import ProjectScoresheet.BoxScore.Batting
import ProjectScoresheet.BoxScore.Pitching
import ProjectScoresheet.Game
import ProjectScoresheet.Game.FrameState
import ProjectScoresheet.Game.GameEvent
import ProjectScoresheet.Retrosheet.Events

data BoxScore
  = BoxScore
  { boxScoreHomeBatting :: Batting
  , boxScoreAwayBatting :: Batting
  , boxScoreHomePitching :: Pitching
  , boxScoreAwayPitching :: Pitching
  } deriving (Eq, Show)

makeClassy_ ''BoxScore

initialBoxScore :: BoxScore
initialBoxScore = BoxScore initialBatting initialBatting initialPitching initialPitching

generateBoxScore :: Game -> BoxScore
generateBoxScore = foldl' (flip updateBoxScore) initialBoxScore . gameEvents

updateBoxScore :: GameEvent -> BoxScore -> BoxScore
updateBoxScore (GameEvent (StartEventType startEvent) _ _) bs = processStartEvent startEvent bs
updateBoxScore (GameEvent (SubEventType subEvent) _ _) bs = processSubEvent subEvent bs
updateBoxScore (GameEvent (PlayEventType pe) _ fs) bs = processPlayEvent pe fs bs
updateBoxScore _ bss = bss

processInfoEvent :: InfoEvent -> Game -> Game
processInfoEvent InfoEvent{..} = do
  let info = Just infoEventValue
  case infoEventKey of
    "visteam" -> _gameAwayTeam .~ info
    "hometeam" -> _gameHomeTeam .~ info
    "date" -> _gameDate .~ info
    "starttime" -> _gameStartTime .~ info
    _ -> id

processStartEvent :: StartEvent -> BoxScore -> BoxScore
processStartEvent StartEvent{..} =
  let
    _batting = case startEventPlayerHome of
      Away -> _boxScoreAwayBatting
      Home -> _boxScoreHomeBatting
    _pitching = case startEventPlayerHome of
      Away -> _boxScoreAwayPitching
      Home -> _boxScoreHomePitching
  in
    over _batting (addPlayerToBatting startEventPlayer startEventBattingPosition) .
    over _pitching (addPlayerToPitching startEventPlayer startEventFieldingPosition)

processSubEvent :: SubEvent -> BoxScore -> BoxScore
processSubEvent SubEvent{..} = 
  let
    _batting = case subEventPlayerHome of
      Away -> _boxScoreAwayBatting
      Home -> _boxScoreHomeBatting
    _pitching = case subEventPlayerHome of
      Away -> _boxScoreAwayPitching
      Home -> _boxScoreHomePitching
  in
    over _batting (addPlayerToBatting subEventPlayer subEventBattingPosition) .
    over _pitching (addPlayerToPitching subEventPlayer subEventFieldingPosition)

processPlayEvent :: PlayEvent -> FrameState -> BoxScore -> BoxScore
processPlayEvent event state = 
  case playEventHomeOrAway event of
    Away -> _boxScoreAwayBatting %~ updateBattingWithPlay event state
    Home -> _boxScoreHomeBatting %~ updateBattingWithPlay event state

prettyPrintBoxScore :: BoxScore -> Text
prettyPrintBoxScore BoxScore{..} = 
  "Away\n"
  <> prettyPrintBatting boxScoreAwayBatting
  <> prettyPrintPitching boxScoreAwayPitching
  <> "Home\n"
  <> prettyPrintBatting boxScoreHomeBatting 
  <> prettyPrintPitching boxScoreHomePitching
