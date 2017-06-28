{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

module ProjectScoresheet.BaseballTypes where

import ClassyPrelude
import Data.Csv
import Data.Hashable
import qualified Data.HashMap.Strict as HashMap

import Closed

data InningHalf = TopInningHalf | BottomInningHalf deriving (Eq, Show)

data HomeOrAway = Away | Home deriving (Eq, Show)

data Base
  = HomePlate
  | FirstBase
  | SecondBase
  | ThirdBase
  deriving (Eq, Show, Enum, Ord)

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

instance Hashable FieldingPosition where
  hashWithSalt = hashUsing fromEnum

type FieldingLineup = HashMap FieldingPosition Text

type BattingOrderPosition = Closed 0 9
type BattingOrder = HashMap BattingOrderPosition Text

initialFieldingLineup :: FieldingLineup
initialFieldingLineup = HashMap.empty

addToFieldingLineup :: Text -> FieldingPosition -> FieldingLineup -> FieldingLineup
addToFieldingLineup playerId fieldingPos fieldingLineup = HashMap.insert fieldingPos playerId fieldingLineup

initialBattingOrder :: BattingOrder
initialBattingOrder = HashMap.empty

addToBattingOrder :: Text -> BattingOrderPosition -> BattingOrder -> BattingOrder
addToBattingOrder playedId battingOrderPosition battingOrder = HashMap.insert battingOrderPosition playedId battingOrder

-- Unfunk me
fieldPositionFromId :: Int -> FieldingPosition
fieldPositionFromId fpId = toEnum (fpId - 1)

instance FromField HomeOrAway where
  parseField "0" = pure Away
  parseField "1" = pure Home
  parseField val = fail $ "Unrecognized value: " ++ show val

instance FromField FieldingPosition where
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
