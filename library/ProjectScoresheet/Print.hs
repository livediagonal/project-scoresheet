{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module ProjectScoresheet.Print 
( prettyPrintBoxScore
) where

import ClassyPrelude hiding (tail, intercalate)
import Data.List (tail)
import ProjectScoresheet.BaseballTypes
import ProjectScoresheet.BoxScore
import qualified Data.HashMap.Strict as HashMap


prettyPrintBoxScore :: BoxScore -> Text
prettyPrintBoxScore BoxScore{..} =
  unlines
    [ "------------------------------------"
    , "Home Batters    AB R H RBI BB SO LOB"
    , "------------------------------------"
    , prettyPrintBattingOrderMap boxScoreHomeBattingOrderMap boxScoreStats
    , "------------------------------------"
    , "Away Batters    AB R H RBI BB SO LOB"
    , "------------------------------------"
    , prettyPrintBattingOrderMap boxScoreAwayBattingOrderMap boxScoreStats
    ]

prettyPrintBattingOrderMap :: BattingOrderMap -> HashMap Text BattingLine -> Text
prettyPrintBattingOrderMap bom counts =
  unlines $ map (\i ->
    let
      battingLineForSlot :: [Text]
      battingLineForSlot = bom HashMap.! i
    in
      tshow (fromIntegral i :: Integer) <> ": " <> prettyPrintBattingLines (reverse $ map (counts HashMap.!) battingLineForSlot)
  ) $ tail [(minBound :: BattingOrderPosition) ..]

prettyPrintBattingLines :: [BattingLine] -> Text
prettyPrintBattingLines [] = ""
prettyPrintBattingLines [x] = prettyPrintBattingLine x
prettyPrintBattingLines (x:xs) = prettyPrintBattingLines xs <> "\n   " <> prettyPrintBattingLine x

prettyPrintBattingLine :: BattingLine -> Text
prettyPrintBattingLine BattingLine{..} = battingLinePlayerId
  <> "      "
  <> tshow battingLineAtBats
  <> " " <> tshow battingLineRuns
  <> " " <> tshow battingLineHits
  <> "   " <> tshow battingLineRBI
  <> "  " <> tshow battingLineWalks
  <> "  " <> tshow battingLineStrikeouts
  <> "  " <> tshow battingLineLOB
