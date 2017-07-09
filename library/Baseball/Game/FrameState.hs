{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Baseball.Game.FrameState
  ( FrameState(..)
  , initialFrameState
  , updateFrameState
  , runnerOnBase
  ) where

import ClassyPrelude
import Control.Lens
import Data.List (elemIndex)

import Baseball.BaseballTypes
import Baseball.Game.GameState
import Baseball.Event
import qualified Data.HashMap.Strict as HashMap

data FrameState
  = FrameState
   { frameStateOuts :: !Int
   , frameStateBatterId :: !(Maybe Text)
   , frameStateRunnerOnFirstId :: !(Maybe Text)
   , frameStateRunnerOnSecondId :: !(Maybe Text)
   , frameStateRunnerOnThirdId :: !(Maybe Text)
  } deriving (Eq, Show)

makeClassy_ ''FrameState

initialFrameState :: FrameState
initialFrameState = FrameState 0 Nothing Nothing Nothing Nothing

updateFrameState :: Event -> GameState -> FrameState -> FrameState
updateFrameState (SubstitutionEvent sub) gs fs = processSubstitution sub gs fs
updateFrameState (PlayEvent play) gs fs = processPlay play gs fs

processPlay :: Play -> GameState -> FrameState -> FrameState
processPlay p@(Play playerId _ _ movements) _ fs =
  fs
  & _frameStateBatterId .~ Just playerId
  & frameState %~ \state -> foldl' (applyRunnerMovement playerId) state movements
  & _frameStateOuts %~ (if isBatterOut p then (+1) else id)

processSubstitution :: Substitution -> GameState -> FrameState -> FrameState
processSubstitution Substitution{..} GameState{..} fs@FrameState{..} =
  let
    replacedBatter :: Text
    replacedBatter = case subTeam of
      Away -> gameStateAwayBattingOrder HashMap.! subBattingPosition
      Home -> gameStateHomeBattingOrder HashMap.! subBattingPosition
  in
    fs
    & over frameState (replaceRunner replacedBatter subPlayer)

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

-- debugEventInFrame :: Event -> FrameState -> FrameState
-- debugEventInFrame (PlayEvent playedid actions _ movements)) fs =
--   trace (show playerId ++ " - " ++ show actions ++ " - " ++ show movements) (debugFrameState fs)
-- debugEventInFrame _ fs = fs

-- debugFrameState :: FrameState -> FrameState
-- debugFrameState fs@FrameState{..} = trace (unlines $ ("Outs: " ++ show frameStateOuts) : catMaybes
--   [ (("1: " ++) . show) <$> frameStateRunnerOnFirstId
--   , (("2: " ++) . show) <$> frameStateRunnerOnSecondId
--   , (("3: " ++) . show) <$> frameStateRunnerOnThirdId
--   ]) fs
