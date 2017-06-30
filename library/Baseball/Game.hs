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
import Retrosheet.Events
import Retrosheet.Parser

data Game
  = Game
  { gameHomeTeam :: !(Maybe Text)
  , gameAwayTeam :: !(Maybe Text)
  , gameDate :: !(Maybe Text)
  , gameStartTime :: !(Maybe Text)
  , gameEvents :: ![GameEvent]
  } deriving (Eq, Show)

makeClassy_ ''Game

processInfoEvent :: InfoEvent -> Game -> Game
processInfoEvent InfoEvent{..} = do
  let info = Just infoEventValue
  case infoEventKey of
    "visteam" -> _gameAwayTeam .~ info
    "hometeam" -> _gameHomeTeam .~ info
    "date" -> _gameDate .~ info
    "starttime" -> _gameStartTime .~ info
    _ -> id

initialGame :: Event -> Game
initialGame event = Game Nothing Nothing Nothing Nothing [initialGameEvent event]

gamesFromFilePath :: String -> IO [Game]
gamesFromFilePath file = do
  events <- retrosheetEventsFromFile file
  pure $ reverse (foldl' (flip generateGames) [] events)

generateGames :: Event -> [Game] -> [Game]
generateGames event@(IdEventType _) games = initialGame event : games
generateGames (InfoEventType info) (g:rest) = processInfoEvent info g : rest
generateGames event (g:rest) = addEventToGame event g : rest
generateGames _ games = games

addEventToGame :: Event -> Game -> Game
addEventToGame event g =
  let
    previousGameEvent = last $ gameEvents g
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
