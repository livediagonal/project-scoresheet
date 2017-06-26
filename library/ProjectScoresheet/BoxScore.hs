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
import ProjectScoresheet.Game
import ProjectScoresheet.Game.FrameState
import ProjectScoresheet.Game.GameEvent
import ProjectScoresheet.Retrosheet.Events

data BoxScore
  = BoxScore
  { boxScoreBatting :: Batting } deriving (Eq, Show)

makeClassy_ ''BoxScore

initialBoxScore :: BoxScore
initialBoxScore = BoxScore initialBatting

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
processStartEvent StartEvent{..} bs = bs
  & _boxScoreBatting %~ addPlayerToBatting startEventPlayerHome startEventPlayer startEventBattingPosition

processSubEvent :: SubEvent -> BoxScore -> BoxScore
processSubEvent SubEvent{..} bs = bs
  & _boxScoreBatting %~ addPlayerToBatting subEventPlayerHome subEventPlayer subEventBattingPosition

processPlayEvent :: PlayEvent -> FrameState -> BoxScore -> BoxScore
processPlayEvent event state = _boxScoreBatting %~ updateBattingWithPlay event state

prettyPrintBoxScore :: BoxScore -> Text
prettyPrintBoxScore BoxScore{..} =
  prettyPrintBatting boxScoreBatting
