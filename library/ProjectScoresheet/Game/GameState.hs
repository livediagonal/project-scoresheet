{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module ProjectScoresheet.Game.GameState
  ( GameState
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
processPlayEvent PlayEvent{..} gs =
  gs & _gameStateInning .~ playEventInning
  & _gameStateInningState .~ case playEventHomeOrAway of 
    Away -> TopInningHalf
    Home -> BottomInningHalf

processStartEvent :: StartEvent -> GameState -> GameState
processStartEvent StartEvent{..} gs = 
  case startEventPlayerHome of
    Away -> gs & _gameStateAwayBattingOrder %~ addToBattingOrder startEventPlayer startEventBattingPosition
      & _gameStateAwayLineup %~ addToFieldingLineup startEventPlayer startEventFieldingPosition
    Home -> gs & _gameStateHomeBattingOrder %~ addToBattingOrder startEventPlayer startEventBattingPosition
      & _gameStateHomeLineup %~ addToFieldingLineup startEventPlayer startEventFieldingPosition

processSubEvent :: SubEvent -> GameState -> GameState
processSubEvent SubEvent{..} gs = 
  case subEventPlayerHome of
    Away -> gs & _gameStateAwayBattingOrder %~ addToBattingOrder subEventPlayer subEventBattingPosition
      & _gameStateAwayLineup %~ addToFieldingLineup subEventPlayer subEventFieldingPosition
    Home -> gs & _gameStateHomeBattingOrder %~ addToBattingOrder subEventPlayer subEventBattingPosition
      & _gameStateHomeLineup %~ addToFieldingLineup subEventPlayer subEventFieldingPosition
      