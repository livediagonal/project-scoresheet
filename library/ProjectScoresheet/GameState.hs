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

updateGameState :: Event -> GameState -> GameState
updateGameState (StartEventType StartEvent{..}) =
  updateOrders startEventPlayerHome startEventPlayer startEventBattingPosition startEventFieldingPosition
updateGameState (SubEventType SubEvent{..}) =
  updateOrders subEventPlayerHome subEventPlayer subEventBattingPosition subEventFieldingPosition
updateGameState _ = id

