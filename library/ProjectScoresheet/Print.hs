{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module ProjectScoresheet.Print where

import ClassyPrelude hiding (tail)
import Closed
import Data.List (tail)
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
    [ "Home:"
    , prettyPrintTeamBoxScore boxScoreStats boxScoreHome
    , "Away:"
    , prettyPrintTeamBoxScore boxScoreStats boxScoreAway
    ]

prettyPrintTeamBoxScore :: BoxScoreCounts -> TeamBoxScore -> Text
prettyPrintTeamBoxScore counts TeamBoxScore{..} =
  unlines
    [ "Batting: H"
    , prettyPrintBattingLines counts batting
    ]

prettyPrintBattingLines :: BoxScoreCounts -> BattingLines -> Text
prettyPrintBattingLines counts battingLines =
  unlines $ map (\i -> tshow (getClosed i) <> ": " <> prettyPrintBattingLineList counts (battingLines HashMap.! i)) $ tail [(minBound :: BattingOrderPosition) ..]

prettyPrintBattingLineList :: BoxScoreCounts -> [BattingLine] -> Text
prettyPrintBattingLineList counts battingLines =
  unlines $ map (prettyPrintBattingLine counts) battingLines

prettyPrintBattingLine :: BoxScoreCounts -> BattingLine -> Text
prettyPrintBattingLine (BoxScoreCounts hits rbis) BattingLine{..} = battingLinePlayerId
  <> " " <> tshow (HashMap.lookupDefault 0 battingLinePlayerId hits)
  <> " " <> tshow (HashMap.lookupDefault 0 battingLinePlayerId rbis)


