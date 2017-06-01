{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module ProjectScoresheet.BaseballTypes where

import ClassyPrelude
import Data.Finite
import Data.Csv

data InningHalf = TopInningHalf | BottomInningHalf deriving (Eq, Show)

data HomeOrAway = Away | Home deriving (Eq, Show)

instance FromField HomeOrAway where
  parseField "0" = pure Away
  parseField "1" = pure Home
  parseField val = fail $ "Unrecognized value: " ++ show val

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

instance FromField FieldPosition where
  parseField "1" = pure Pitcher
  parseField "2" = pure Catcher
  parseField "3" = pure FirstBaseman
  parseField "4" = pure SecondBaseman
  parseField "5" = pure ThirdBaseman
  parseField "6" = pure ShortStop
  parseField "7" = pure LeftFielder
  parseField "8" = pure CenterFielder
  parseField "9" = pure RightFielder
  parseField "10" = pure DesignatedHitter
  parseField "11" = pure PinchHitter
  parseField "12" = pure PinchRunner
  parseField val = fail $ "Unrecognized value: " ++ show val

newtype BattingPosition
  = BattingPosition
  { battingPosition :: Finite 10
  } deriving (Eq, Show)

instance FromField BattingPosition where
  parseField "0" = pure $ BattingPosition 0
  parseField "1" = pure $ BattingPosition 1
  parseField "2" = pure $ BattingPosition 2
  parseField "3" = pure $ BattingPosition 3
  parseField "4" = pure $ BattingPosition 4
  parseField "5" = pure $ BattingPosition 5
  parseField "6" = pure $ BattingPosition 6
  parseField "7" = pure $ BattingPosition 7
  parseField "8" = pure $ BattingPosition 8
  parseField "9" = pure $ BattingPosition 9
  parseField val = fail $ "Unrecognized value: " ++ show val

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


