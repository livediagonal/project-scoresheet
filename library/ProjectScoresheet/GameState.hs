{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module ProjectScoresheet.GameState where

import ClassyPrelude hiding (toLower)
import Control.Lens
import ProjectScoresheet.BaseballTypes
import ProjectScoresheet.EventTypes
import ProjectScoresheet.PlayResult

data GameState
  = GameState
  { gameStateHomeBattingOrder :: BattingOrder
  , gameStateAwayBattingOrder:: BattingOrder
  , gameStateHomeFieldingLineup :: FieldingLineup
  , gameStateAwayFieldingLineup :: FieldingLineup
  , gameStateInning :: !Int
  , gameStateInningHalf :: InningHalf
  , gameStateOuts :: !Int
  , gameStateIsPinchHit :: !Bool
  , gameStateBatterId :: !(Maybe Text)
  , gameStateBattingTeam :: !(Maybe HomeOrAway)
  , gameStatePitcherId :: !(Maybe Text)
  , gameStateRunnerOnFirstId :: !(Maybe Text)
  , gameStateRunnerOnSecondId :: !(Maybe Text)
  , gameStateRunnerOnThirdId :: !(Maybe Text)
  } deriving (Eq, Show)

data Game
  = Game
  { gameHomeTeam :: !(Maybe Text)
  , gameAwayTeam :: !(Maybe Text)
  , gameDate :: !(Maybe Text)
  , gameStartTime :: !(Maybe Text)
  , gameGameState :: GameState
  , gameLastPlay :: !(Maybe PlayResult)
  } deriving (Eq, Show)

makeClassy_ ''GameState
makeClassy_ ''Game

unstartedGameState :: GameState
unstartedGameState = GameState emptyBattingOrder emptyBattingOrder emptyFieldingLineup emptyFieldingLineup 1 BottomInningHalf 0 False Nothing Nothing Nothing Nothing Nothing Nothing

unstartedGame :: Game
unstartedGame = Game Nothing Nothing Nothing Nothing unstartedGameState Nothing

data EventWithContext = EventWithContext Event GameState deriving (Eq, Show)

initialContext :: EventWithContext
initialContext = EventWithContext EmptyEvent unstartedGameState

updateOrders :: HomeOrAway -> Text -> BattingOrderPosition -> FieldingPositionId -> GameState -> GameState
updateOrders hoa player battingPosition fieldingPosition gs =
  case hoa of
    Away -> gs
      & _gameStateAwayBattingOrder %~ addToBattingOrder player battingPosition
      & _gameStateAwayFieldingLineup %~ addToFieldingLineup player fieldingPosition
    Home -> gs
      & _gameStateHomeBattingOrder %~ addToBattingOrder player battingPosition
      & _gameStateHomeFieldingLineup %~ addToFieldingLineup player fieldingPosition

removePlayerFromBase :: Base -> GameState -> GameState
removePlayerFromBase base = addPlayerToBase Nothing base

addPlayerToBase :: Maybe Text -> Base -> GameState -> GameState
addPlayerToBase playerId base =
  case base of
    FirstBase -> _gameStateRunnerOnFirstId .~ playerId
    SecondBase -> _gameStateRunnerOnSecondId .~ playerId
    ThirdBase -> _gameStateRunnerOnThirdId .~ playerId
    _ -> id

playerOnBase :: Text -> Base -> GameState -> Maybe Text
playerOnBase batterId base GameState{..} =
  case base of
    FirstBase -> gameStateRunnerOnFirstId
    SecondBase -> gameStateRunnerOnSecondId
    ThirdBase -> gameStateRunnerOnThirdId
    HomePlate -> Just batterId

baseForPlayer :: Text -> GameState -> Maybe Base
baseForPlayer playerId GameState{..} =
  find (\base -> (== Just playerId) $ case base of
    FirstBase -> gameStateRunnerOnFirstId
    SecondBase -> gameStateRunnerOnSecondId
    ThirdBase -> gameStateRunnerOnThirdId
    _ -> Nothing) [FirstBase, SecondBase, ThirdBase]

applyRunnerMovement :: Text -> GameState -> PlayMovement -> GameState
applyRunnerMovement _ gs (PlayMovement startBase _ False) =
  removePlayerFromBase startBase gs
  & _gameStateOuts %~ (+1)
applyRunnerMovement batterId gs (PlayMovement startBase endBase True) = gs
  & gameState %~ addPlayerToBase (playerOnBase batterId startBase gs) endBase
  & gameState %~ removePlayerFromBase startBase

resetInningState :: GameState -> GameState
resetInningState state = state
  & _gameStateRunnerOnFirstId .~ Nothing
  & _gameStateRunnerOnSecondId .~ Nothing
  & _gameStateRunnerOnThirdId .~ Nothing
  & _gameStateOuts .~ 0

batterOuts :: [Out] -> Int
batterOuts outs = length $ filter (\o -> case o of Strikeout _ -> True; RoutinePlay _ Nothing -> True; _ -> False) outs

applyAction :: PlayAction -> GameState -> GameState
applyAction (Outs outs) gs = gs & _gameStateOuts %~ (+ batterOuts outs)
applyAction _ gs = gs

updateGameState :: Event -> GameState -> GameState
updateGameState (StartEventType StartEvent{..}) =
  updateOrders startEventPlayerHome startEventPlayer startEventBattingPosition startEventFieldingPosition
updateGameState (SubEventType SubEvent{..}) =
  updateOrders subEventPlayerHome subEventPlayer subEventBattingPosition subEventFieldingPosition
updateGameState (PlayEventType (PlayEvent _ _ playerId _ _ (PlayResult action _ movements))) =
  gameState %~ \state -> foldl' (applyRunnerMovement playerId) state movements
  & gameState %~ applyAction action
  & gameState %~ \state' ->
    if gameStateOuts state' == 3
    then resetInningState state'
    else state'
updateGameState _ = id
