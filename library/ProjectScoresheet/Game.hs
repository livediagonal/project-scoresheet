{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module ProjectScoresheet.Game where

import ClassyPrelude hiding (toLower, last)
import Control.Lens
import Data.List (last)
import ProjectScoresheet.BaseballTypes
import ProjectScoresheet.Retrosheet.Events
import ProjectScoresheet.Retrosheet.Parser
import ProjectScoresheet.Play

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

data GameState
  = GameState
  { gameStateInning :: Int
  , gameStateInningState :: InningHalf
  , gameStateHomeLineup :: FieldingLineup
  , gameStateAwayLineup :: FieldingLineup
  , gameStateHomeBattingOrder :: BattingOrder
  , gameStateAwayBattingOrder :: BattingOrder
  } deriving (Eq, Show)

data FrameState
  = FrameState
  { frameStateOuts :: !Int
  , frameStateBatterId :: !(Maybe Text)
  , frameStatePitcherId :: !(Maybe Text)
  , frameStateRunnerOnFirstId :: !(Maybe Text)
  , frameStateRunnerOnSecondId :: !(Maybe Text)
  , frameStateRunnerOnThirdId :: !(Maybe Text)
  } deriving (Eq, Show)

makeClassy_ ''Game
makeClassy_ ''GameState
makeClassy_ ''FrameState

initialGame :: Event -> Game
initialGame event = Game Nothing Nothing Nothing Nothing [initialGameEvent event]

initialGameEvent :: Event -> GameEvent
initialGameEvent event = GameEvent event initialGameState initialFrameState

initialGameState :: GameState
initialGameState = GameState 0 BottomInningHalf initialFieldingLineup initialFieldingLineup initialBattingOrder initialBattingOrder

initialFrameState :: FrameState
initialFrameState = FrameState 0 Nothing Nothing Nothing Nothing Nothing

removePlayerFromBase :: Base -> FrameState -> FrameState
removePlayerFromBase base = addPlayerToBase Nothing base

addPlayerToBase :: Maybe Text -> Base -> FrameState -> FrameState
addPlayerToBase playerId base =
  case base of
    FirstBase -> _frameStateRunnerOnFirstId .~ playerId
    SecondBase -> _frameStateRunnerOnSecondId .~ playerId
    ThirdBase -> _frameStateRunnerOnThirdId .~ playerId
    _ -> id

playerOnBase :: Text -> Base -> FrameState -> Maybe Text
playerOnBase batterId base FrameState{..} =
  case base of
    FirstBase -> frameStateRunnerOnFirstId
    SecondBase -> frameStateRunnerOnSecondId
    ThirdBase -> frameStateRunnerOnThirdId
    HomePlate -> Just batterId

baseForPlayer :: Text -> FrameState -> Maybe Base
baseForPlayer playerId FrameState{..} =
  find (\base -> (== Just playerId) $ case base of
    FirstBase -> frameStateRunnerOnFirstId
    SecondBase -> frameStateRunnerOnSecondId
    ThirdBase -> frameStateRunnerOnThirdId
    _ -> Nothing) [FirstBase, SecondBase, ThirdBase]

applyRunnerMovement :: Text -> FrameState -> PlayMovement -> FrameState
applyRunnerMovement _ gs (PlayMovement startBase _ False) =
  removePlayerFromBase startBase gs
  & _frameStateOuts %~ (+1)
applyRunnerMovement batterId gs (PlayMovement startBase endBase True) = gs
  & frameState %~ addPlayerToBase (playerOnBase batterId startBase gs) endBase
  & frameState %~ removePlayerFromBase startBase

applyAction :: PlayAction -> FrameState -> FrameState
applyAction (Outs outs) gs = gs & _frameStateOuts %~ (+ batterOuts outs)
applyAction _ gs = gs

updateFrameState :: Event -> FrameState -> FrameState
updateFrameState (PlayEventType (PlayEvent _ _ playerId _ _ (Play action _ movements))) =
  frameState %~ \state -> foldl' (applyRunnerMovement playerId) state movements
  & frameState %~ applyAction action
  & frameState %~ \state' ->
    if frameStateOuts state' == 3  then initialFrameState else state'
updateFrameState _ = id

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
    nextGameState = initialGameState -- todo implement
    nextFrameState = updateFrameState (gameEventEvent previousGameEvent) (gameEventFrameState previousGameEvent)
  in
    g & _gameEvents %~ (++ [GameEvent event nextGameState nextFrameState])
