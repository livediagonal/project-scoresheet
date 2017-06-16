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
import Data.Text (intercalate)
import ProjectScoresheet.BaseballTypes
import ProjectScoresheet.GameState
import ProjectScoresheet.BoxScore
import qualified Data.HashMap.Strict as HashMap


prettyPrintBoxScore :: BoxScore -> Text
prettyPrintBoxScore BoxScore{..} =
  unlines
    [ "------------------------------"
    , "Home      AB R H RBI BB SO LOB"
    , "------------------------------"
    , prettyPrintBattingOrderMap boxScoreHomeBattingOrderMap boxScoreStats
    , "------------------------------"
    , "Away      AB R H RBI BB SO LOB"
    , "------------------------------"
    , prettyPrintBattingOrderMap boxScoreAwayBattingOrderMap boxScoreStats
    ]

prettyPrintBattingOrderMap :: BattingOrderMap -> HashMap Text BattingLine -> Text
prettyPrintBattingOrderMap bom counts =
  unlines $ map (\i ->
    let
      battingLineForSlot :: [Text]
      battingLineForSlot = bom HashMap.! i
    in
      prettyPrintBattingLines $ map (counts HashMap.!) battingLineForSlot
  ) $ tail [(minBound :: BattingOrderPosition) ..]

prettyPrintBattingLines :: [BattingLine] -> Text
prettyPrintBattingLines [] = ""
prettyPrintBattingLines [x] = prettyPrintBattingLine True x
prettyPrintBattingLines (x:xs) = prettyPrintBattingLine False x <> "\n" <> prettyPrintBattingLines xs

prettyPrintBattingLine :: Bool -> BattingLine -> Text
prettyPrintBattingLine isFirst BattingLine{..} = (if isFirst then "" else " ")
  <> battingLinePlayerId
  <> (if isFirst then "   " else "  ")
  <> tshow battingLineAtBats
  <> " " <> tshow battingLineRuns
  <> " " <> tshow battingLineHits
  <> "   " <> tshow battingLineRBI
  <> "  " <> tshow battingLineWalks
  <> "  " <> tshow battingLineStrikeouts
  <> "  " <> tshow battingLineLOB


prettyPrintGame :: Game -> Text
prettyPrintGame Game{..} =
  unlines
    [ tshow (fromMaybe "" gameAwayTeam) <> "@" <> tshow (fromMaybe "" gameHomeTeam)
    , ""
    , prettyPrintGameState gameGameState
    ]

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

