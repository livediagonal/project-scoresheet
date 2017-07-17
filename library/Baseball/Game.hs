{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Baseball.Game where

import ClassyPrelude hiding (last)
import Control.Lens
import Data.List (last)

import Baseball.Game.GameEvent
import Baseball.Event

data Game
  = Game
  { gameHomeTeam :: !(Maybe Text)
  , gameAwayTeam :: !(Maybe Text)
  , gameDate :: !(Maybe Text)
  , gameStartTime :: !(Maybe Text)
  , gameEvents :: ![GameEvent]
  }

makeClassy_ ''Game

initialGame :: Game
initialGame = Game Nothing Nothing Nothing Nothing []

addEventToGame :: Event -> Game -> Game
addEventToGame event g =
  let
    previousGameEvent =
      if null (gameEvents g)
        then initialGameEvent event
        else last $ gameEvents g
  in
    g & _gameEvents %~ (++ [nextGameEvent event previousGameEvent])

prettyPrintGameInfo :: Game -> Text
prettyPrintGameInfo Game{..} =
  unlines $ catMaybes
    [ do
        home <- gameHomeTeam
        away <- gameAwayTeam
        pure $ away <> "@" <> home
    , gameDate
    , gameStartTime
    ]
