{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module ProjectScoresheet.Print where

import ClassyPrelude hiding (tail, intercalate)
import Closed
import Data.List (tail)
import Data.Text (intercalate)
import ProjectScoresheet.BaseballTypes
import ProjectScoresheet.GameState
import ProjectScoresheet.BoxScore
import qualified Data.HashMap.Strict as HashMap

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

prettyPrintBoxScore :: BoxScore -> Text
prettyPrintBoxScore BoxScore{..} =
  unlines
    [ "Home"
    , "-------------------"
    , "Batters   AB R H RBI W SO"
    , "-------------------"
    , prettyPrintBattingLines boxScoreHomeBattingOrderMap boxScoreStats
    , "Away"
    , "-------------------"
    , "Batters   AB R H RBI W SO"
    , "-------------------"
    , prettyPrintBattingLines boxScoreAwayBattingOrderMap boxScoreStats
    ]

prettyPrintBattingLines :: BattingOrderMap -> BoxScoreCounts -> Text
prettyPrintBattingLines bom counts =
  unlines $ map (\i ->
    let
      battingLineForSlot :: [Text]
      battingLineForSlot = bom HashMap.! i
    in
      intercalate "\n " (map (prettyPrintBattingLine counts) battingLineForSlot)
  ) $ tail [(minBound :: BattingOrderPosition) ..]

prettyPrintBattingLine :: BoxScoreCounts -> Text -> Text
prettyPrintBattingLine (BoxScoreCounts atBats hits rbis runs walks strikeouts) player = player
  <> "   " <> tshow (HashMap.lookupDefault 0 player atBats)
  <> " " <> tshow (HashMap.lookupDefault 0 player runs)
  <> " " <> tshow (HashMap.lookupDefault 0 player hits)
  <> "   " <> tshow (HashMap.lookupDefault 0 player rbis)
  <> " " <> tshow (HashMap.lookupDefault 0 player walks)
  <> "  " <> tshow (HashMap.lookupDefault 0 player strikeouts)
