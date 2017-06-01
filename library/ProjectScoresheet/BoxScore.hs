{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module ProjectScoresheet.BoxScore where

import ClassyPrelude
import ProjectScoresheet.BaseballTypes
import ProjectScoresheet.PlayResult
import qualified Data.HashMap.Strict as HashMap

data BoxScore
  = BoxScore
  { homeBoxScore :: TeamBoxScore
  , awayBoxScore :: TeamBoxScore
  } deriving (Eq, Show)

initialBoxScore :: BoxScore
initialBoxScore = BoxScore initialTeamBoxScore initialTeamBoxScore

addPlayToBoxScore :: Text -> Text -> PlayResult -> BoxScore -> BoxScore
addPlayToBoxScore _ _ _ boxScore = boxScore

addPlayerToBoxScore :: HomeOrAway -> Text -> BattingOrderPosition -> FieldingPositionId -> BoxScore -> BoxScore
addPlayerToBoxScore homeOrAway playerId battingPosition fieldingPosition boxScore@BoxScore{..} =
  case homeOrAway of
    Away -> boxScore { awayBoxScore = addPlayerToTeamBoxScore playerId battingPosition fieldingPosition awayBoxScore }
    Home -> boxScore { homeBoxScore = addPlayerToTeamBoxScore playerId battingPosition fieldingPosition homeBoxScore }

data TeamBoxScore
  = TeamBoxScore
  { innings :: [InningLine]
  , batting :: BattingLines
  , pitching :: [PitchingLine]
  } deriving (Eq, Show)

initialTeamBoxScore :: TeamBoxScore
initialTeamBoxScore = TeamBoxScore [] initialBattingLines []

addPlayerToTeamBoxScore :: Text -> BattingOrderPosition -> FieldingPositionId -> TeamBoxScore -> TeamBoxScore
addPlayerToTeamBoxScore playerId battingLineId fieldingPosition teamBoxScore@TeamBoxScore{..} =
    teamBoxScore
    { batting = addPlayerToBatting playerId battingLineId batting
    , pitching = addPlayerToPitching playerId fieldingPosition pitching
    }

addPlayerToBatting :: Text -> BattingOrderPosition -> BattingLines -> BattingLines
addPlayerToBatting _ 0 battingLines = battingLines
addPlayerToBatting playerId battingLineId battingLines =
  let
    initialPlayerBattingLine = initialBattingLine playerId
  in
    case HashMap.lookup battingLineId battingLines of
      Nothing -> HashMap.insert battingLineId [initialPlayerBattingLine] battingLines
      Just battingLineList ->
        case battingLineContains playerId battingLineList of
          True -> battingLines
          False -> HashMap.insert battingLineId (battingLineList ++ [initialPlayerBattingLine]) battingLines

battingLineContains :: Text -> [BattingLine] -> Bool
battingLineContains playerId = any (\bl -> battingLinePlayerId bl == playerId)

type BattingLines = HashMap BattingOrderPosition [BattingLine]

initialBattingLines :: BattingLines
initialBattingLines = HashMap.fromList $ zip [minBound ..] $ repeat []

data InningLine
  = InningLine
  { hits :: !Int
  , runs :: !Int
  , errors :: !Int
  } deriving (Eq, Show)

initialInningLine :: InningLine
initialInningLine = InningLine 0 0 0

data BattingLine
  = BattingLine
  { battingLinePlayerId :: !Text
  , battingLineAtBats :: !Int
  , battingLinePlateAppearances :: !Int
  , battingLineHits :: !Int
  , battingLineRuns :: !Int
  , battingLineSingles :: !Int
  , battingLineDoubles :: !Int
  , battingLineTriples :: !Int
  , battingLineHomeRuns :: !Int
  , battingLineGrandSlams :: !Int
  , battingLineRunsBattingIn :: !Int
  , battingLineTwoOutRunsBattingIn :: !Int
  , battingLineLeftOnBase :: !Int
  , battingLineWalks :: !Int
  , battingLineIntentionalWalks :: !Int
  , battingLineStrikeOuts :: !Int
  , battingLineGroundIntoDoublePlays :: !Int
  , battingLineSacrificeBunts :: !Int
  , battingLineSacrificeFlys :: !Int
  , battingLineHitByPitches :: !Int
  , battingLineStolenBases :: !Int
  , battingLineCaughtStealing :: !Int
  , battingLineReachedOnErrors :: !Int
  } deriving (Eq, Show)

initialBattingLine :: Text -> BattingLine
initialBattingLine playerId = BattingLine playerId 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

data PitchingLine
  = PitchingLine
  { playerId :: !Text
  , strikes :: !Int
  } deriving (Eq, Show)

initialPitchingLine :: Text -> PitchingLine
initialPitchingLine playerId = PitchingLine playerId 0

addPlayerToPitching :: Text -> FieldingPositionId -> [PitchingLine] -> [PitchingLine]
addPlayerToPitching playerId 1 pitching =
  pitching ++ [initialPitchingLine playerId]
addPlayerToPitching _ _ pitching = pitching
