{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module ProjectScoresheet.BoxScore where

import ClassyPrelude
import ProjectScoresheet.BaseballTypes
import ProjectScoresheet.PlayResult

data BoxScore
  = BoxScore
  { homeBoxScore :: TeamBoxScore
  , awayBoxScore :: TeamBoxScore
  } deriving (Eq, Show)

initialBoxScore :: BoxScore
initialBoxScore = BoxScore initialTeamBoxScore initialTeamBoxScore

addPlayToBoxScore :: Text -> Text -> PlayResult -> BoxScore -> BoxScore
addPlayToBoxScore _ _ _ boxScore = boxScore

addPlayerToBoxScore :: HomeOrAway -> Text -> BattingPosition -> FieldPosition -> BoxScore -> BoxScore
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

addPlayerToTeamBoxScore :: Text -> BattingPosition -> FieldPosition -> TeamBoxScore -> TeamBoxScore
addPlayerToTeamBoxScore playerId battingPosition fieldingPosition teamBoxScore@TeamBoxScore{..} = 
    teamBoxScore 
    { batting = addPlayerToBatting playerId battingPosition batting
    , pitching = addPlayerToPitching playerId fieldingPosition pitching
    }

addPlayerToBatting :: Text -> BattingPosition -> BattingLines -> BattingLines
addPlayerToBatting playerId (BattingPosition 1) battingLines = 
    let
      battingLineList = battingLinesSpotOne battingLines
    in
      if battingLineExistsForPlayer playerId battingLineList
        then battingLines
        else battingLines { battingLinesSpotOne = battingLineList ++ [initialBattingLine playerId] }
addPlayerToBatting playerId (BattingPosition 2) battingLines = 
    let
      battingLineList = battingLinesSpotTwo battingLines
    in
      if battingLineExistsForPlayer playerId battingLineList
        then battingLines
        else battingLines { battingLinesSpotTwo = battingLineList ++ [initialBattingLine playerId] }
addPlayerToBatting playerId (BattingPosition 3) battingLines = 
    let
      battingLineList = battingLinesSpotThree battingLines
    in
      if battingLineExistsForPlayer playerId battingLineList
        then battingLines
        else battingLines { battingLinesSpotThree = battingLineList ++ [initialBattingLine playerId] }

addPlayerToBatting playerId (BattingPosition 4) battingLines = 
    let
      battingLineList = battingLinesSpotFour battingLines
    in
      if battingLineExistsForPlayer playerId battingLineList
        then battingLines
        else battingLines { battingLinesSpotFour = battingLineList ++ [initialBattingLine playerId] }
addPlayerToBatting playerId (BattingPosition 5) battingLines = 
    let
      battingLineList = battingLinesSpotFive battingLines
    in
      if battingLineExistsForPlayer playerId battingLineList
        then battingLines
        else battingLines { battingLinesSpotFive = battingLineList ++ [initialBattingLine playerId] }
addPlayerToBatting playerId (BattingPosition 6) battingLines = 
    let
      battingLineList = battingLinesSpotSix battingLines
    in
      if battingLineExistsForPlayer playerId battingLineList
        then battingLines
        else battingLines { battingLinesSpotSix = battingLineList ++ [initialBattingLine playerId] }
addPlayerToBatting playerId (BattingPosition 7) battingLines = 
    let
      battingLineList = battingLinesSpotSeven battingLines
    in
      if battingLineExistsForPlayer playerId battingLineList
        then battingLines
        else battingLines { battingLinesSpotSeven = battingLineList ++ [initialBattingLine playerId] }
addPlayerToBatting playerId (BattingPosition 8) battingLines = 
    let
      battingLineList = battingLinesSpotEight battingLines
    in
      if battingLineExistsForPlayer playerId battingLineList
        then battingLines
        else battingLines { battingLinesSpotEight = battingLineList ++ [initialBattingLine playerId] }
addPlayerToBatting playerId (BattingPosition 9) battingLines = 
    let
      battingLineList = battingLinesSpotNine battingLines
    in
      if battingLineExistsForPlayer playerId battingLineList
        then battingLines
        else battingLines { battingLinesSpotNine = battingLineList ++ [initialBattingLine playerId] }
addPlayerToBatting _ _ battingLines = battingLines

battingLineExistsForPlayer :: Text -> [BattingLine] -> Bool
battingLineExistsForPlayer playerId battingLineList = any (\p -> battingLinePlayedId p == playerId) battingLineList

data BattingLines
  = BattingLines
  { battingLinesSpotOne :: [BattingLine]
  , battingLinesSpotTwo :: [BattingLine]
  , battingLinesSpotThree :: [BattingLine]
  , battingLinesSpotFour :: [BattingLine]
  , battingLinesSpotFive :: [BattingLine]
  , battingLinesSpotSix :: [BattingLine]
  , battingLinesSpotSeven :: [BattingLine]
  , battingLinesSpotEight :: [BattingLine]
  , battingLinesSpotNine :: [BattingLine]
  } deriving (Eq, Show)

initialBattingLines :: BattingLines
initialBattingLines = BattingLines [] [] [] [] [] [] [] [] []

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
  { battingLinePlayedId :: !Text
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

addPlayerToPitching :: Text -> FieldPosition -> [PitchingLine] -> [PitchingLine]
addPlayerToPitching playerId Pitcher pitching = 
  pitching ++ [initialPitchingLine playerId]
addPlayerToPitching _ _ pitching = pitching


-- data FieldingSummary
--   = FieldingSummary
--   { fieldingSummaryOuts :: !Int
--   , fieldingSummaryPutOuts :: !Int
--   , fieldingSummaryAssists :: !Int
--   , fieldingSummaryOutfieldAssists :: !Int
--   , fieldingSummaryErrors :: !Int
--   , fieldingSummaryDoublePlays :: !Int
--   , fieldingSummaryTriplePlays :: !Int
--   , fieldingSummaryPassedBalls :: !Int
--   , fieldingSummaryInningsPlayed :: !Int
--   , fieldingSummaryTotalChanges :: !Int
--   }