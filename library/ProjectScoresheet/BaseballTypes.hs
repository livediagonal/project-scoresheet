{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module ProjectScoresheet.BaseballTypes where

import ClassyPrelude
import Data.Csv
import qualified Data.HashMap.Strict as HashMap

import Closed

data InningHalf = TopInningHalf | BottomInningHalf deriving (Eq, Show)

data HomeOrAway = Away | Home deriving (Eq, Show)
instance FromField HomeOrAway where
  parseField "0" = pure Away
  parseField "1" = pure Home
  parseField val = fail $ "Unrecognized value: " ++ show val

data Base
  = FirstBase
  | SecondBase
  | ThirdBase
  | HomePlate
  deriving (Eq, Show, Enum)

data FieldingPosition
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

fieldPositionFromId :: Int -> FieldingPosition
fieldPositionFromId fpId = toEnum (fpId - 1)

type FieldingPositionId = Closed 1 12
type FieldingLineup = HashMap FieldingPositionId Text

initialFieldingLineup :: FieldingLineup
initialFieldingLineup = HashMap.empty

addToFieldingLineup :: Text -> FieldingPositionId -> FieldingLineup -> FieldingLineup
addToFieldingLineup playerId fieldingId fieldingLineup = HashMap.insert fieldingId playerId fieldingLineup

type BattingOrderPosition = Closed 0 9
type BattingOrder = HashMap BattingOrderPosition Text

initialBattingOrder :: BattingOrder
initialBattingOrder = HashMap.empty

addToBattingOrder :: Text -> BattingOrderPosition -> BattingOrder -> BattingOrder
addToBattingOrder _ 0 battingOrder = battingOrder
addToBattingOrder playedId battingOrderPosition battingOrder = HashMap.insert battingOrderPosition playedId battingOrder
