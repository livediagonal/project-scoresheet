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
  , gameStateBattingTeam :: !(Maybe HomeOrAway)
  , gameStatePitcherId :: !(Maybe Text)
  , gameStateRunnerOnFirstId :: !(Maybe Text)
  , gameStateRunnerOnSecondId :: !(Maybe Text)
  , gameStateRunnerOnThirdId :: !(Maybe Text)
  , gameStateCurrentBatterId :: !(Maybe Text)
  , gameStateCurrentPitcherId :: !(Maybe Text)
  , gameStateRunnerOnFirstResponsiblePitcherId :: !(Maybe Text)
  , gameStateRunnerOnSecondResponsiblePitcherId :: !(Maybe Text)
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
unstartedGameState = GameState emptyBattingOrder emptyBattingOrder emptyFieldingLineup emptyFieldingLineup 1 BottomInningHalf 0 0 0 False False Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

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

applyRunnerMovement :: Text -> PlayMovement -> GameState -> GameState
applyRunnerMovement _ (PlayMovement startBase _ False) gs =
  let
    removedPlayerState@GameState{..} = removePlayerFromBase startBase gs
  in
    removedPlayerState { gameStateOuts = gameStateOuts + 1 }
applyRunnerMovement batterId (PlayMovement startBase endBase True) gs =
  let
    playerId = playerOnBase batterId startBase gs
  in
    removePlayerFromBase startBase $ addPlayerToBase playerId endBase gs

resetInningState :: GameState -> GameState
resetInningState state = state
  { gameStateRunnerOnFirstId = Nothing
  , gameStateRunnerOnSecondId = Nothing
  , gameStateRunnerOnThirdId = Nothing
  , gameStateOuts = 0
  }

checkForStolenBase :: Text -> PlayAction -> GameState -> GameState
{-checkForStolenBase playerId (StolenBase base) state =
  let
    Just currentBase = baseForPlayer playerId state -- Crash if not found, something is broken
    player = Just playerId
  in
    case base of
      FirstBase -> addPlayerToBase player base $ removePlayerFromBase currentBase state
      SecondBase -> addPlayerToBase player base $ removePlayerFromBase currentBase state
      ThirdBase -> addPlayerToBase player base $ removePlayerFromBase currentBase state
      HomePlate -> removePlayerFromBase currentBase state-}
checkForStolenBase _ _ state = state

applyRunnerHit :: Text -> Base -> GameState -> GameState
applyRunnerHit playerId FirstBase state = state { gameStateRunnerOnFirstId = Just playerId }
applyRunnerHit playerId SecondBase state = state { gameStateRunnerOnSecondId = Just playerId }
applyRunnerHit playerId ThirdBase state = state { gameStateRunnerOnThirdId = Just playerId }
applyRunnerHit _ _ state = state

batterOuts :: [Out] -> Int
batterOuts outs = length $ filter (\o -> case o of Strikeout _ -> True; RoutinePlay _ -> True; _ -> False) outs

applyAction :: Text -> PlayAction -> GameState -> GameState
applyAction playerId (Hit base _) state = applyRunnerHit playerId base state
applyAction playerId (Walk _) state = state { gameStateRunnerOnFirstId = Just playerId }
applyAction playerId HitByPitch state = state { gameStateRunnerOnFirstId = Just playerId }
applyAction playerId (Outs outs) state =
  let
    stateWithRunners@GameState{..} =
      case any (\out -> case out of FieldersChoice _ -> True; _ -> False) outs of
        True -> state { gameStateRunnerOnFirstId = Just playerId }
        False -> state
  in
    stateWithRunners { gameStateOuts = gameStateOuts + batterOuts outs }
applyAction _ _ state = state

updateGameState :: Event -> GameState -> GameState
updateGameState (StartEventType StartEvent{..}) =
  updateOrders startEventPlayerHome startEventPlayer startEventBattingPosition startEventFieldingPosition
updateGameState (SubEventType SubEvent{..}) =
  updateOrders subEventPlayerHome subEventPlayer subEventBattingPosition subEventFieldingPosition
updateGameState (PlayEventType (PlayEvent _ _ playerId _ _ (PlayResult action _ movements))) =
  \state ->
    let
      stateWithStolenBases = checkForStolenBase playerId action state
      stateWithMovements = foldr (applyRunnerMovement playerId) stateWithStolenBases (reverse movements)
      stateWithActions = applyAction playerId action stateWithMovements
    in
      if gameStateOuts stateWithActions == 3 then resetInningState stateWithActions else stateWithActions
updateGameState _ = id

