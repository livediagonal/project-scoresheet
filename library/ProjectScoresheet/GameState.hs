{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ProjectScoresheet.GameState where

import ClassyPrelude
import ProjectScoresheet.PlayResult
import ProjectScoresheet.BoxScore
import ProjectScoresheet.RawTypes

data InningHalf = TopInningHalf | BottomInningHalf deriving (Eq, Show)

data FieldPosition
  = Pitcher
  | Catcher
  | FirstBaseman
  | SecondBaseman
  | ThirdBaseman
  | ShortStop
  | LeftFielder
  | CenterFielder
  | RightFielder
  | DesignatedHitter
  | PinchHitter
  | PinchRunner
  deriving (Eq, Show, Enum)

fieldPositionFromId :: Int -> FieldPosition
fieldPositionFromId fpId = toEnum (fpId - 1)

data BattingOrder
  = BattingOrder
  { battingOrderSpotOnePlayerId :: !(Maybe Text)
  , battingOrderSpotTwoPlayerId :: !(Maybe Text)
  , battingOrderSpotThreePlayerId :: !(Maybe Text)
  , battingOrderSpotFourPlayerId :: !(Maybe Text)
  , battingOrderSpotFivePlayerId :: !(Maybe Text)
  , battingOrderSpotSixPlayerId :: !(Maybe Text)
  , battingOrderSpotSevenPlayerId :: !(Maybe Text)
  , battingOrderSpotEightPlayerId :: !(Maybe Text)
  , battingOrderSpotNinePlayerId :: !(Maybe Text)
  } deriving (Eq, Show)

emptyBattingOrder :: BattingOrder
emptyBattingOrder = BattingOrder Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

addToBattingOrder :: Text -> BattingPosition -> BattingOrder -> BattingOrder
addToBattingOrder playedId (BattingPosition 1) battingOrder = battingOrder { battingOrderSpotOnePlayerId = Just playedId }
addToBattingOrder playedId (BattingPosition 2) battingOrder = battingOrder { battingOrderSpotTwoPlayerId = Just playedId }
addToBattingOrder playedId (BattingPosition 3) battingOrder = battingOrder { battingOrderSpotThreePlayerId = Just playedId }
addToBattingOrder playedId (BattingPosition 4) battingOrder = battingOrder { battingOrderSpotFourPlayerId = Just playedId }
addToBattingOrder playedId (BattingPosition 5) battingOrder = battingOrder { battingOrderSpotFivePlayerId = Just playedId }
addToBattingOrder playedId (BattingPosition 6) battingOrder = battingOrder { battingOrderSpotSixPlayerId = Just playedId }
addToBattingOrder playedId (BattingPosition 7) battingOrder = battingOrder { battingOrderSpotSevenPlayerId = Just playedId }
addToBattingOrder playedId (BattingPosition 8) battingOrder = battingOrder { battingOrderSpotEightPlayerId = Just playedId }
addToBattingOrder playedId (BattingPosition 9) battingOrder = battingOrder { battingOrderSpotNinePlayerId = Just playedId }
addToBattingOrder _ _ battingOrder = battingOrder

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
  { gameStateHomeBattingOrder :: BattingOrder
  , gameStateAwayBattingOrder:: BattingOrder
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
unstartedGameState = GameState emptyBattingOrder emptyBattingOrder 1 BottomInningHalf 0 0 0 False False Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
