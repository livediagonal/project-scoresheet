{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ProjectScoresheet.GameState where

import ClassyPrelude
import ProjectScoresheet.BaseballTypes
import ProjectScoresheet.BoxScore
import ProjectScoresheet.PlayResult

data Game
  = Game
  { gameHomeTeam :: !(Maybe Text)
  , gameAwayTeam :: !(Maybe Text)
  , gameDate :: !(Maybe Text)
  , gameStartTime :: !(Maybe Text)
  , gameBoxScore :: BoxScore
  , gameState :: GameState
  , gameLastPlay :: !(Maybe PlayResult)
  }

unstartedGame :: Game
unstartedGame = Game Nothing Nothing Nothing Nothing initialBoxScore unstartedGameState Nothing

data GameState
  = GameState
  { gameStateHomeLineup :: Lineup
  , gameStateAwayLineup :: Lineup
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

unstartedGameState :: GameState
unstartedGameState = GameState emptyLineup emptyLineup 1 BottomInningHalf 0 0 0 False False Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
