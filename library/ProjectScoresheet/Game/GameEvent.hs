{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module ProjectScoresheet.Game.GameEvent
  ( GameEvent(..)
  , initialGameEvent
  , nextGameEvent
  ) where

import ClassyPrelude
import Control.Lens

import ProjectScoresheet.Game.GameState
import ProjectScoresheet.Game.FrameState
import ProjectScoresheet.Retrosheet.Events

data GameEvent =
  GameEvent
  { gameEventEvent :: !Event
  , gameEventGameState :: !GameState
  , gameEventFrameState :: !FrameState
  } deriving (Eq, Show)

makeClassy_ ''GameEvent

initialGameEvent :: Event -> GameEvent
initialGameEvent event = GameEvent event initialGameState initialFrameState

nextGameEvent :: Event -> GameEvent -> GameEvent
nextGameEvent event previousGameEvent@GameEvent{..} =
  previousGameEvent &
  _gameEventEvent .~ event &
  _gameEventGameState %~ updateGameState gameEventEvent &
  _gameEventFrameState %~ updateFrameState gameEventEvent
