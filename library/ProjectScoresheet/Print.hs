{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module ProjectScoresheet.Print where

import ClassyPrelude
import ProjectScoresheet.BaseballTypes
import ProjectScoresheet.GameState

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