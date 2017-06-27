{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module ProjectScoresheet.Game.FrameState
  ( FrameState(..)
  , initialFrameState
  , updateFrameState
  , runnerOnBase
  ) where

import ClassyPrelude
import Control.Lens
import Data.List (elemIndex)

import ProjectScoresheet.BaseballTypes
import ProjectScoresheet.Game.GameState
import ProjectScoresheet.Play
import ProjectScoresheet.Retrosheet.Events
import qualified Data.HashMap.Strict as HashMap

data FrameState
  = FrameState
   { frameStateOuts :: !Int
   , frameStateBatterId :: !(Maybe Text)
   , frameStatePitcherId :: !(Maybe Text)
   , frameStateRunnerOnFirstId :: !(Maybe Text)
   , frameStateRunnerOnSecondId :: !(Maybe Text)
   , frameStateRunnerOnThirdId :: !(Maybe Text)
  } deriving (Eq, Show)

makeClassy_ ''FrameState

initialFrameState :: FrameState
initialFrameState = FrameState 0 Nothing Nothing Nothing Nothing Nothing

debugEventInFrame :: Event -> FrameState -> FrameState
debugEventInFrame (PlayEventType (PlayEvent _ _ playerId _ _ (Play actions _ movements))) fs =
  trace (show playerId ++ " - " ++ show actions ++ " - " ++ show movements) (debugFrameState fs)
debugEventInFrame _ fs = fs

debugFrameState :: FrameState -> FrameState
debugFrameState fs@FrameState{..} = trace (unlines $ ("Outs: " ++ show frameStateOuts) : catMaybes
  [ (("1: " ++) . show) <$> frameStateRunnerOnFirstId
  , (("2: " ++) . show) <$> frameStateRunnerOnSecondId
  , (("3: " ++) . show) <$> frameStateRunnerOnThirdId
  ]) fs

updateFrameState :: Event -> GameState -> FrameState -> FrameState
updateFrameState e@(PlayEventType (PlayEvent _ _ playerId _ _ p@(Play _ _ movements))) gs fs =
  fs
  & frameState %~ \state -> foldl' (applyRunnerMovement playerId) state movements
  & _frameStateOuts %~ (if isBatterOut p then (+1) else id)
  & frameState %~ \state' ->
    if frameStateOuts state' == 3
    then initialFrameState
    else state'
updateFrameState (SubEventType SubEvent{..}) GameState{..} fs@FrameState{..} =
  let
    replacedBatter :: Text
    replacedBatter = case subEventPlayerHome of
      Away -> gameStateAwayBattingOrder HashMap.! subEventBattingPosition
      Home -> gameStateHomeBattingOrder HashMap.! subEventBattingPosition
  in
    fs
    & over frameState (replaceRunner replacedBatter subEventPlayer)
updateFrameState _ _ fs = fs

replaceRunner :: Text -> Text -> FrameState -> FrameState
replaceRunner originalRunner newRunner fs =
  case baseForRunner originalRunner fs of
    Nothing -> fs
    Just base -> addPlayerToBase (Just newRunner) base fs

runnerOnBase :: Base -> FrameState -> Maybe Text
runnerOnBase FirstBase = frameStateRunnerOnFirstId
runnerOnBase SecondBase = frameStateRunnerOnSecondId
runnerOnBase ThirdBase = frameStateRunnerOnThirdId
runnerOnBase _ = const Nothing

applyRunnerMovement :: Text -> FrameState -> PlayMovement -> FrameState
applyRunnerMovement _ gs (PlayMovement HomePlate _ False) = gs -- Hack: need cleaner way of not double-counting batter outs
applyRunnerMovement _ gs (PlayMovement startBase _ False) =
  removePlayerFromBase startBase gs
  & _frameStateOuts %~ (+1)
applyRunnerMovement batterId gs (PlayMovement startBase endBase True) = gs
  & frameState %~ addPlayerToBase (playerOnBase batterId startBase gs) endBase
  & frameState %~ removePlayerFromBase startBase

removePlayerFromBase :: Base -> FrameState -> FrameState
removePlayerFromBase base = addPlayerToBase Nothing base

addPlayerToBase :: Maybe Text -> Base -> FrameState -> FrameState
addPlayerToBase playerId base =
  case base of
    FirstBase -> _frameStateRunnerOnFirstId .~ playerId
    SecondBase -> _frameStateRunnerOnSecondId .~ playerId
    ThirdBase -> _frameStateRunnerOnThirdId .~ playerId
    _ -> id

baseForRunner :: Text -> FrameState -> Maybe Base
baseForRunner playerId FrameState{..} =
  case elemIndex (Just playerId) [frameStateRunnerOnFirstId, frameStateRunnerOnSecondId, frameStateRunnerOnThirdId] of
    Just 0 -> Just FirstBase
    Just 1 -> Just SecondBase
    Just 2 -> Just ThirdBase
    _ -> Nothing

playerOnBase :: Text -> Base -> FrameState -> Maybe Text
playerOnBase batterId base FrameState{..} =
  case base of
    FirstBase -> frameStateRunnerOnFirstId
    SecondBase -> frameStateRunnerOnSecondId
    ThirdBase -> frameStateRunnerOnThirdId
    HomePlate -> Just batterId
