{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module ProjectScoresheet.BoxScore where

import ClassyPrelude
import Control.Lens

import ProjectScoresheet.BoxScore.Batting
import ProjectScoresheet.BoxScore.Pitching
import ProjectScoresheet.Game
import ProjectScoresheet.Game.FrameState
import ProjectScoresheet.Game.GameEvent
import ProjectScoresheet.Retrosheet.Events

data BoxScore
  = BoxScore
  { boxScoreBatting :: Batting
  , boxScorePitching :: Pitching
  } deriving (Eq, Show)

makeClassy_ ''BoxScore

initialBoxScore :: BoxScore
initialBoxScore = BoxScore initialBatting initialPitching

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
  over _boxScoreBatting (addPlayerToBatting startEventPlayerHome startEventPlayer startEventBattingPosition) .
  over _boxScorePitching (addPlayerToPitching startEventPlayerHome startEventPlayer startEventFieldingPosition)

processSubEvent :: SubEvent -> BoxScore -> BoxScore
processSubEvent SubEvent{..} = 
  over _boxScoreBatting (addPlayerToBatting subEventPlayerHome subEventPlayer subEventBattingPosition) .
  over _boxScorePitching (addPlayerToPitching subEventPlayerHome subEventPlayer subEventFieldingPosition)

processPlayEvent :: PlayEvent -> FrameState -> BoxScore -> BoxScore
processPlayEvent event state = _boxScoreBatting %~ updateBattingWithPlay event state

prettyPrintBoxScore :: BoxScore -> Text
prettyPrintBoxScore BoxScore{..} =
  prettyPrintBatting boxScoreBatting <> prettyPrintPitching boxScorePitching
