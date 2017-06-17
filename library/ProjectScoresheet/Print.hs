{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module ProjectScoresheet.Print 
( prettyPrintBoxScore
, prettyPrintGameState
) where

import ClassyPrelude hiding (tail, intercalate)
import Data.List (tail)
import ProjectScoresheet.BaseballTypes
import ProjectScoresheet.GameState
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
      tshow (fromIntegral i) <> ": " <> (prettyPrintBattingLines $ reverse $ map (counts HashMap.!) battingLineForSlot)
  ) $ tail [(minBound :: BattingOrderPosition) ..]

prettyPrintBattingLines :: [BattingLine] -> Text
prettyPrintBattingLines [] = ""
prettyPrintBattingLines [x] = prettyPrintBattingLine True x
prettyPrintBattingLines (x:xs) = prettyPrintBattingLines xs <> "\n" <> prettyPrintBattingLine False x

prettyPrintBattingLine :: Bool -> BattingLine -> Text
prettyPrintBattingLine isFirst BattingLine{..} = (if isFirst then "" else "    ")
  <> battingLinePlayerId
  <> (if isFirst then "      " else "     ")
  <> tshow battingLineAtBats
  <> " " <> tshow battingLineRuns
  <> " " <> tshow battingLineHits
  <> "   " <> tshow battingLineRBI
  <> "  " <> tshow battingLineWalks
  <> "  " <> tshow battingLineStrikeouts
  <> "  " <> tshow battingLineLOB


prettyPrintGameState :: GameState -> Text
prettyPrintGameState GameState{..} =
  unlines
    [ "Inning: " <> tshow gameStateInning <> ", Outs: " <> tshow gameStateOuts
    , ""
    , "Away: "
    , prettyPrintBattingOrder gameStateAwayBattingOrder
    , "Home: "
    , prettyPrintBattingOrder gameStateAwayBattingOrder
    ]

prettyPrintBattingOrder :: BattingOrder -> Text
prettyPrintBattingOrder battingOrder =
  unlines $ map (\i -> tshow i <> ": " <> battingOrder HashMap.! i) $ tail [(minBound :: BattingOrderPosition) ..]

