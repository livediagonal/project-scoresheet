{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module ProjectScoresheet.BoxScore where

import ClassyPrelude

data BoxScore
  = BoxScore
  { battingOrder :: [[BattingLine]] } deriving (Eq, Show)

initialBoxScore :: BoxScore
initialBoxScore = BoxScore [[initialBattingLine "placeholder"]]

--   int g, pa, ab, r, h, b2, b3, hr, hrslam, bi, bi2out, bb, ibb, so, gdp, hp, sh, sf, sb, cs, xi;

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
initialBattingLine playedId = BattingLine playedId 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

battingLineTotalBases :: BattingLine -> Int
battingLineTotalBases BattingLine{..} = battingLineSingles + 2 * battingLineDoubles + 3 * battingLineTriples + 4 * battingLineHomeRuns

-- updateBoxScore :: GameState -> BoxScore
-- updateBoxScore gameState BoxScore{..} = 
--   boxScore
--     { battingOrder = updateBattingOrder gameState battingOrder }


-- updateBattingOrder :: GameState -> BattingOrder -> BattingOrder
-- updateBattingOrder GameState{..} battingOrder = 
--   let
--     battingLine = findOrCreatebattingLine gameStateBatterId battingOrder
--   in
--     battingOrder


-- findOrCreatebattingLine :: Text -> BattingOrder -> battingLine
-- findOrCreatebattingLine battedId battingOrder = 
--   let
--     battingLine = find (\battingLine{..} -> playedId == battingLinePlayedId) battingOrder
--   in
--     if isNothing battingLine
--       then initialbattingLine
--       else battingLine



-- possibly add: caught stealing, stolen base attempts
data FieldingSummary
  = FieldingSummary
  { fieldingSummaryOuts :: !Int
  , fieldingSummaryPutOuts :: !Int
  , fieldingSummaryAssists :: !Int
  , fieldingSummaryOutfieldAssists :: !Int
  , fieldingSummaryErrors :: !Int
  , fieldingSummaryDoublePlays :: !Int
  , fieldingSummaryTriplePlays :: !Int
  , fieldingSummaryPassedBalls :: !Int
  , fieldingSummaryInningsPlayed :: !Int
  , fieldingSummaryTotalChanges :: !Int
  }