{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module ProjectScoresheet.Game.GameState
  ( GameState(..)
  , initialGameState
  , updateGameState
  ) where

import ClassyPrelude
import Control.Lens

import ProjectScoresheet.BaseballTypes
import ProjectScoresheet.Retrosheet.Events

data GameState
  = GameState
  { gameStateInning :: Int
  , gameStateInningState :: InningHalf
  , gameStateHomeLineup :: FieldingLineup
  , gameStateAwayLineup :: FieldingLineup
  , gameStateHomeBattingOrder :: BattingOrder
  , gameStateAwayBattingOrder :: BattingOrder
  } deriving (Eq, Show)

makeClassy_ ''GameState

initialGameState :: GameState
initialGameState = GameState 0 TopInningHalf initialFieldingLineup initialFieldingLineup initialBattingOrder initialBattingOrder

updateGameState :: Event -> GameState -> GameState
updateGameState (StartEventType event) = processStartEvent event
updateGameState (PlayEventType event) = processPlayEvent event
updateGameState (SubEventType event) = processSubEvent event
updateGameState _ = id

processPlayEvent :: PlayEvent -> GameState -> GameState
processPlayEvent PlayEvent{..} =
  set _gameStateInning playEventInning .
  set _gameStateInningState (case playEventHomeOrAway of Away -> TopInningHalf; Home -> BottomInningHalf)

processStartEvent :: StartEvent -> GameState -> GameState
processStartEvent StartEvent{..} =
  case startEventPlayerHome of
    Away ->
      over _gameStateAwayBattingOrder (addToBattingOrder startEventPlayer startEventBattingPosition) .
      over _gameStateAwayLineup (addToFieldingLineup startEventPlayer startEventFieldingPosition)
    Home ->
      over _gameStateHomeBattingOrder (addToBattingOrder startEventPlayer startEventBattingPosition) .
      over _gameStateHomeLineup (addToFieldingLineup startEventPlayer startEventFieldingPosition)

processSubEvent :: SubEvent -> GameState -> GameState
processSubEvent SubEvent{..} =
  case subEventPlayerHome of
    Away ->
      over _gameStateAwayBattingOrder (addToBattingOrder subEventPlayer subEventBattingPosition) .
      over _gameStateAwayLineup (addToFieldingLineup subEventPlayer subEventFieldingPosition)
    Home ->
      over _gameStateHomeBattingOrder (addToBattingOrder subEventPlayer subEventBattingPosition) .
      over _gameStateHomeLineup (addToFieldingLineup subEventPlayer subEventFieldingPosition)
