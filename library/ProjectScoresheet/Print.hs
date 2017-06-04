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
    , prettyPrintTeamBoxScore boxScoreHome
    , "Away:"
    , prettyPrintTeamBoxScore boxScoreAway
    ]

prettyPrintTeamBoxScore :: TeamBoxScore -> Text
prettyPrintTeamBoxScore TeamBoxScore{..} =
  unlines
    [ "Batting: H"
    , prettyPrintBattingLines batting
    ]

prettyPrintBattingLines :: BattingLines -> Text
prettyPrintBattingLines battingLines =
  unlines $ map (\i -> tshow (getClosed i) <> ": " <> prettyPrintBattingLineList (battingLines HashMap.! i)) $ tail [(minBound :: BattingOrderPosition) ..]

prettyPrintBattingLineList :: [BattingLine] -> Text
prettyPrintBattingLineList battingLines =
  unlines $ map prettyPrintBattingLine battingLines

prettyPrintBattingLine :: BattingLine -> Text
prettyPrintBattingLine BattingLine{..} = battingLinePlayerId <> " " <> tshow battingLineHits


