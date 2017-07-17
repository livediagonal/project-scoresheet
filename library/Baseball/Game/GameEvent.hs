{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Baseball.Game.GameEvent
  ( GameEvent(..)
  , initialGameEvent
  , nextGameEvent
  , prettyPrintGameEvent
  ) where

import ClassyPrelude
import Control.Lens

import Baseball.Game.GameState
import Baseball.Game.FrameState
import Baseball.Event

data GameEvent
  = GameEvent
  { gameEventEvent :: !Event
  , gameEventGameState :: !GameState
  , gameEventFrameState :: !FrameState
  } deriving (Eq)

makeClassy_ ''GameEvent

initialGameEvent :: Event -> GameEvent
initialGameEvent event = GameEvent event initialGameState initialFrameState

nextGameEvent :: Event -> GameEvent -> GameEvent
nextGameEvent event previousGameEvent@GameEvent{..} =
  let
    updatedFrameState = updateFrameState gameEventEvent gameEventGameState gameEventFrameState
  in
    if frameStateOuts updatedFrameState == 3
      then
        previousGameEvent &
        _gameEventEvent .~ event &
        _gameEventFrameState .~ initialFrameState &
        _gameEventGameState %~ advanceHalfInning
      else
        previousGameEvent &
        _gameEventEvent .~ event &
        _gameEventFrameState .~ updatedFrameState &
        _gameEventGameState %~ updateGameState gameEventEvent

prettyPrintGameEvent :: GameEvent -> Text
prettyPrintGameEvent GameEvent{..} = unlines
  [ prettyPrintFrameState gameEventFrameState
  , "Last play: " ++ tshow gameEventEvent
  ]
