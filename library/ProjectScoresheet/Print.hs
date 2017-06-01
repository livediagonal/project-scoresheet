{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module ProjectScoresheet.Print where

import ClassyPrelude
import ProjectScoresheet.BaseballTypes
import ProjectScoresheet.GameState
import ProjectScoresheet.BoxScore

prettyPrintGame :: Game -> Text
prettyPrintGame Game{..} =
  unlines
    [ tshow (fromMaybe "" gameAwayTeam) <> "@" <> tshow (fromMaybe "" gameHomeTeam)
    , ""
    , prettyPrintGameState gameState
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
prettyPrintBattingOrder BattingOrder{..} =
  unlines
    [ "1: " <> maybe "" tshow battingOrderSpotOnePlayerId
    , "2: " <> maybe "" tshow battingOrderSpotTwoPlayerId
    , "3: " <> maybe "" tshow battingOrderSpotThreePlayerId
    , "4: " <> maybe "" tshow battingOrderSpotFourPlayerId
    , "5: " <> maybe "" tshow battingOrderSpotFivePlayerId
    , "6: " <> maybe "" tshow battingOrderSpotSixPlayerId
    , "7: " <> maybe "" tshow battingOrderSpotSevenPlayerId
    , "8: " <> maybe "" tshow battingOrderSpotEightPlayerId
    , "9: " <> maybe "" tshow battingOrderSpotNinePlayerId
    ]

prettyPrintBoxScore :: BoxScore -> Text
prettyPrintBoxScore BoxScore{..} =
  unlines
    [ "Home:"
    , prettyPrintTeamBoxScore homeBoxScore
    , "Away:"
    , prettyPrintTeamBoxScore awayBoxScore
    ]

prettyPrintTeamBoxScore :: TeamBoxScore -> Text
prettyPrintTeamBoxScore TeamBoxScore{..} =
  unlines 
    [ "Batting: H"
    , prettyPrintBattingLines batting
    -- , prettyPrintPitching pitching
    ]

prettyPrintBattingLines :: BattingLines -> Text
prettyPrintBattingLines BattingLines{..} =
  unlines
    [ "1: " <> prettyPrintBattingLineList battingLinesSpotOne
    , "2: " <> prettyPrintBattingLineList battingLinesSpotTwo
    , "3: " <> prettyPrintBattingLineList battingLinesSpotThree
    , "4: " <> prettyPrintBattingLineList battingLinesSpotFour
    , "5: " <> prettyPrintBattingLineList battingLinesSpotFive
    , "6: " <> prettyPrintBattingLineList battingLinesSpotSix
    , "7: " <> prettyPrintBattingLineList battingLinesSpotSeven
    , "8: " <> prettyPrintBattingLineList battingLinesSpotEight
    , "9: " <> prettyPrintBattingLineList battingLinesSpotNine
    ]


prettyPrintBattingLineList :: [BattingLine] -> Text
prettyPrintBattingLineList battingLines =
  unlines $ map (prettyPrintBattingLine) battingLines

prettyPrintBattingLine :: BattingLine -> Text
prettyPrintBattingLine BattingLine{..} = battingLinePlayedId <> " " <> tshow battingLineHits 


