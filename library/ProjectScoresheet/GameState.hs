{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ProjectScoresheet.GameState where

import ClassyPrelude
import ProjectScoresheet.BaseballTypes
import ProjectScoresheet.BoxScore
import ProjectScoresheet.PlayResult
import ProjectScoresheet.RawTypes

data FieldingLineup
  = FieldingLineup
  { fieldingLineupPitcher :: !(Maybe Text)
  , fieldingLineupCatcher :: !(Maybe Text)
  , fieldingLineupFirstBaseman :: !(Maybe Text)
  , fieldingLineupSecondBaseman :: !(Maybe Text)
  , fieldingLineupThirdBaseman :: !(Maybe Text)
  , fieldingLineupShortstop :: !(Maybe Text)
  , fieldingLineupLeftFielder :: !(Maybe Text)
  , fieldingLineupCenterFielder :: !(Maybe Text)
  , fieldingLineupRightFielder :: !(Maybe Text)
  , fieldingLineupDesignatedHitter :: !(Maybe Text)
  } deriving (Eq, Show)

emptyFieldingLineup :: FieldingLineup
emptyFieldingLineup = FieldingLineup Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

addToFieldingLineup :: Text -> FieldPosition -> FieldingLineup -> FieldingLineup
addToFieldingLineup playedId Pitcher fieldingLineup = fieldingLineup { fieldingLineupPitcher = Just playedId }
addToFieldingLineup playedId Catcher fieldingLineup = fieldingLineup { fieldingLineupCatcher = Just playedId }
addToFieldingLineup playedId FirstBaseman fieldingLineup = fieldingLineup { fieldingLineupFirstBaseman = Just playedId }
addToFieldingLineup playedId SecondBaseman fieldingLineup = fieldingLineup { fieldingLineupSecondBaseman = Just playedId }
addToFieldingLineup playedId ThirdBaseman fieldingLineup = fieldingLineup { fieldingLineupThirdBaseman = Just playedId }
addToFieldingLineup playedId ShortStop fieldingLineup = fieldingLineup { fieldingLineupShortstop = Just playedId }
addToFieldingLineup playedId LeftFielder fieldingLineup = fieldingLineup { fieldingLineupLeftFielder = Just playedId }
addToFieldingLineup playedId CenterFielder fieldingLineup = fieldingLineup { fieldingLineupCenterFielder = Just playedId }
addToFieldingLineup playedId RightFielder fieldingLineup = fieldingLineup { fieldingLineupRightFielder = Just playedId }
addToFieldingLineup playedId DesignatedHitter fieldingLineup = fieldingLineup { fieldingLineupDesignatedHitter = Just playedId }
addToFieldingLineup _ _ fieldingLineup = fieldingLineup

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
  } deriving (Eq, Show)

unstartedGame :: Game
unstartedGame = Game Nothing Nothing Nothing Nothing initialBoxScore unstartedGameState Nothing

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

unstartedGameState :: GameState
unstartedGameState = GameState emptyBattingOrder emptyBattingOrder emptyFieldingLineup emptyFieldingLineup 1 BottomInningHalf 0 0 0 False False Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
