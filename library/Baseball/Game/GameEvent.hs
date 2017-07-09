{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Baseball.Game.GameEvent
  ( GameEvent(..)
  , initialGameEvent
  , nextGameEvent
  , toCsv
  ) where

import ClassyPrelude
import Control.Lens
import qualified Data.ByteString.Lazy as BL
import Data.Csv

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

toCsv :: [GameEvent] -> BL.ByteString
toCsv ges = encodeWith (defaultEncodeOptions {encQuoting = QuoteNone}) ges

instance ToRecord GameEvent where
  toRecord (GameEvent (PlayEvent play) gs fs) = record
    [ toField (gameStateInning gs)
    , toField (frameStateOuts fs)
    , toField (show $ playPlayer play)
    ]

  toRecord (GameEvent (SubstitutionEvent Substitution{..}) gs fs) = record
    [ toField (gameStateInning gs)
    , toField (frameStateOuts fs)
    , toField (show subPlayer)
    ]
