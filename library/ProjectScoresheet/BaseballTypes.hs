{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ProjectScoresheet.BaseballTypes where

import ClassyPrelude

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
