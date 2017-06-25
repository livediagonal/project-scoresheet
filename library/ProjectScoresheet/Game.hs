{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module ProjectScoresheet.Game where

import ClassyPrelude hiding (toLower, last)
import Control.Lens
import Data.List (last)
import ProjectScoresheet.Retrosheet.Events
import ProjectScoresheet.Retrosheet.Parser
import ProjectScoresheet.Game.GameState
import ProjectScoresheet.Game.FrameState

data Game
  = Game
  { gameHomeTeam :: !(Maybe Text)
  , gameAwayTeam :: !(Maybe Text)
  , gameDate :: !(Maybe Text)
  , gameStartTime :: !(Maybe Text)
  , gameEvents :: ![GameEvent]
  } deriving (Eq, Show)

data GameEvent = 
  GameEvent 
  { gameEventEvent :: Event
  , gameEventGameState :: GameState
  , gameEventFrameState :: FrameState 
  } deriving (Eq, Show)

makeClassy_ ''Game

initialGame :: Event -> Game
initialGame event = Game Nothing Nothing Nothing Nothing [initialGameEvent event]

initialGameEvent :: Event -> GameEvent
initialGameEvent event = GameEvent event initialGameState initialFrameState

gamesFromFilePath :: String -> IO [Game]
gamesFromFilePath file = do
  events <- retrosheetEventsFromFile file
  pure $ reverse (foldl' (flip generateGames) [] events)

generateGames :: Event -> [Game] -> [Game]
generateGames event@(IdEventType _) games = initialGame event : games
generateGames event (g:rest) = addEventToGame event g : rest
generateGames _ games = games

addEventToGame :: Event -> Game -> Game
addEventToGame event g@Game{..} = 
  let
    previousGameEvent = last gameEvents
    nextGameState = updateGameState (gameEventEvent previousGameEvent) (gameEventGameState previousGameEvent)
    nextFrameState = updateFrameState (gameEventEvent previousGameEvent) (gameEventFrameState previousGameEvent)
  in
    g & _gameEvents %~ (++ [traceShow nextGameState (GameEvent event nextGameState nextFrameState)])
