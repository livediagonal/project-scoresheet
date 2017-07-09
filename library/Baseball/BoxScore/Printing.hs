{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module Baseball.BoxScore.Printing where

import ClassyPrelude hiding (replicate)
import Control.Lens hiding ((.=))
import Data.Csv
import qualified Data.HashMap.Strict as HashMap
import Data.Text (replicate)
import Generics.Deriving.Monoid hiding ((<>))

import Baseball.BaseballTypes
import Baseball.Game.FrameState
import Baseball.Play
import Retrosheet.Events

prettyPrintBatting :: Batting -> Text
prettyPrintBatting Batting{..} =
  unlines
    [ "-------------------------------------------"
    , "Batters          AB   R   H RBI  BB  SO LOB"
    , "-------------------------------------------"
    , prettyPrintBattingOrderMap battingOrder battingStats
    ]

prettyPrintBattingOrderMap :: BattingOrderMap -> HashMap Text BattingLine -> Text
prettyPrintBattingOrderMap bom counts =
  let
    (_:hitters) = [(minBound :: BattingOrderPosition) ..]
    battingLines = reverse $ map (counts HashMap.!) $ concatMap (bom HashMap.!) hitters
    battingTotals = mconcat battingLines
    playerLines = unlines $ map (\i ->
        tshow (fromIntegral i :: Integer) <> ": "
        <> prettyPrintBattingLines (map (counts HashMap.!) (bom HashMap.! i))
      ) hitters
  in
    playerLines <> prettyPrintBattingTotals battingTotals

prettyPrintBattingTotals :: BattingLine -> Text
prettyPrintBattingTotals bl =
  unlines
    [ "-------------------------------------------"
    , prettyPrintBattingLine (set _battingLinePlayerId "Total:     " bl)
    ]

prettyPrintBattingLines :: [BattingLine] -> Text
prettyPrintBattingLines [] = ""
prettyPrintBattingLines [x] = prettyPrintBattingLine x
prettyPrintBattingLines (x:xs) = prettyPrintBattingLines xs <> "\n   " <> prettyPrintBattingLine x

prettyPrintBattingLine :: BattingLine -> Text
prettyPrintBattingLine BattingLine{..} = battingLinePlayerId
  <> "    "
  <> prettyColumn (tshow battingLineAtBats)
  <> prettyColumn (tshow battingLineRuns)
  <> prettyColumn (tshow battingLineHits)
  <> prettyColumn (tshow battingLineRBI)
  <> prettyColumn (tshow battingLineWalks)
  <> prettyColumn (tshow battingLineStrikeouts)
  <> prettyColumn (tshow battingLineLOB)

prettyColumn :: Text -> Text
prettyColumn t = replicate (4 - length t) " " <>  t
