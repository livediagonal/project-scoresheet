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

data LineupSlot
  = LineupSlot
  { lineupSlotPlayerId :: !Text
  , lineupSlotFieldPosition :: FieldPosition
  } deriving (Eq, Show)

data Lineup
  = Lineup
  { lineupSlotOne :: Maybe LineupSlot
  , lineupSlotTwo :: Maybe LineupSlot
  , lineupSlotThree :: Maybe LineupSlot
  , lineupSlotFour :: Maybe LineupSlot
  , lineupSlotFive :: Maybe LineupSlot
  , lineupSlotSix :: Maybe LineupSlot
  , lineupSlotSeven :: Maybe LineupSlot
  , lineupSlotEight :: Maybe LineupSlot
  , lineupSlotNine :: Maybe LineupSlot
  } deriving (Eq, Show)

emptyLineup :: Lineup
emptyLineup = Lineup Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
