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
  , gameStateHomeRuns :: !Int
  , gameStateAwayRuns :: !Int
  , gameStateOuts :: !Int
  , gameStateIsLeadOff :: !Bool
  , gameStateIsPinchHit :: !Bool
  , gameStateBatterId :: !(Maybe Text)
  , gameStatePitcherId :: !(Maybe Text)
  , gameStateRunnerOnFirstId :: !(Maybe Text)
  , gameStateRunnerOnSecondId :: !(Maybe Text)
  , gameStateRunnerOnThirdId :: !(Maybe Text)
  , gameStateCurrentBatterId :: !(Maybe Text)
  , gameStateCurrentPitcherId :: !(Maybe Text)
  , gameStateRunnerOnFirstResponsiblePitcherId :: !(Maybe Text)
  , gameStateRunnerOnSeocndResponsiblePitcherId :: !(Maybe Text)
  , gameStateRunnerOnThirdResponsiblePitcherId :: !(Maybe Text)
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
unstartedGameState = GameState emptyBattingOrder emptyBattingOrder emptyFieldingLineup emptyFieldingLineup 1 BottomInningHalf 0 0 0 False False Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

unstartedGame :: Game
unstartedGame = Game Nothing Nothing Nothing Nothing unstartedGameState Nothing

data EventWithContext = EventWithContext Event GameState deriving (Eq, Show)

initialContext :: EventWithContext
initialContext = EventWithContext EmptyEvent unstartedGameState

updateOrders :: HomeOrAway -> Text -> BattingOrderPosition -> FieldingPositionId -> GameState -> GameState
updateOrders hoa player battingPosition fieldingPosition =
  case hoa of
    Away ->
      over _gameStateAwayBattingOrder (addToBattingOrder player battingPosition)
      . over _gameStateAwayFieldingLineup (addToFieldingLineup player fieldingPosition)
    Home ->
      over _gameStateHomeBattingOrder (addToBattingOrder player battingPosition)
      . over _gameStateHomeFieldingLineup (addToFieldingLineup player fieldingPosition)

removePlayerFromBase :: Base -> GameState -> GameState
removePlayerFromBase base gs =
  case base of
    FirstBase -> gs { gameStateRunnerOnFirstId = Nothing }
    SecondBase -> gs { gameStateRunnerOnSecondId = Nothing }
    ThirdBase -> gs { gameStateRunnerOnThirdId = Nothing }
    _ -> gs

addPlayerToBase :: Maybe Text -> Base -> GameState -> GameState
addPlayerToBase playerId base gs =
  case base of
    FirstBase -> gs { gameStateRunnerOnFirstId = playerId }
    SecondBase -> gs { gameStateRunnerOnSecondId = playerId }
    ThirdBase -> gs { gameStateRunnerOnThirdId = playerId }
    _ -> gs

playerOnBase :: Base -> GameState -> Maybe Text
playerOnBase base GameState{..} =
  case base of
    FirstBase -> gameStateRunnerOnFirstId
    SecondBase -> gameStateRunnerOnSecondId
    ThirdBase -> gameStateRunnerOnThirdId
    _ -> Nothing

applyRunnerMovement :: PlayMovement -> GameState -> GameState
applyRunnerMovement (PlayMovement startBase _ False) gs = removePlayerFromBase startBase gs
applyRunnerMovement (PlayMovement startBase endBase True) gs =
  let
    playerId = playerOnBase startBase gs
  in
    addPlayerToBase playerId endBase $ removePlayerFromBase startBase gs

updateGameState :: Event -> GameState -> GameState
updateGameState (StartEventType StartEvent{..}) =
  updateOrders startEventPlayerHome startEventPlayer startEventBattingPosition startEventFieldingPosition
updateGameState (SubEventType SubEvent{..}) =
  updateOrders subEventPlayerHome subEventPlayer subEventBattingPosition subEventFieldingPosition
updateGameState (PlayEventType (PlayEvent _ _ _ _ _ (PlayResult _ _ movements))) =
  \state -> foldr applyRunnerMovement state movements
updateGameState _ = id

