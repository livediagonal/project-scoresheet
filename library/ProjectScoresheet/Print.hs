{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module ProjectScoresheet.Print where

import ClassyPrelude hiding (tail, intercalate)
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
    [ "------------------------------"
    , "Home      AB R H RBI BB SO LOB"
    , "------------------------------"
    , prettyPrintBattingLines boxScoreHomeBattingOrderMap boxScoreStats
    , "------------------------------"
    , "Away      AB R H RBI BB SO LOB"
    , "------------------------------"
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
prettyPrintBattingLine (BoxScoreCounts atBats hits rbis runs walks strikeouts lob) player = player
  <> "   " <> tshow (HashMap.lookupDefault 0 player atBats)
  <> " " <> tshow (HashMap.lookupDefault 0 player runs)
  <> " " <> tshow (HashMap.lookupDefault 0 player hits)
  <> "   " <> tshow (HashMap.lookupDefault 0 player rbis)
  <> "  " <> tshow (HashMap.lookupDefault 0 player walks)
  <> "  " <> tshow (HashMap.lookupDefault 0 player strikeouts)
  <> "  " <> tshow (HashMap.lookupDefault 0 player lob)
