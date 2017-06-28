{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ProjectScoresheet.BoxScore.Batting where

import ClassyPrelude
import Control.Lens hiding ((.=))
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HashMap
import Generics.Deriving.Monoid hiding ((<>))

import ProjectScoresheet.BaseballTypes
import ProjectScoresheet.Game.FrameState
import ProjectScoresheet.Play
import ProjectScoresheet.Retrosheet.Events

type BattingOrderMap = HashMap BattingOrderPosition [Text]

data Batting
  = Batting
  { battingStats :: !(HashMap Text BattingLine)
  , battingHomeOrder :: !BattingOrderMap
  , battingAwayOrder :: !BattingOrderMap
  } deriving (Eq, Show)

data BattingLine
  = BattingLine
  { battingLinePlayerId :: !Text
  , battingLineAtBats :: !Int
  , battingLineRuns :: !Int
  , battingLineHits :: !Int
  , battingLineRBI :: !Int
  , battingLineWalks :: !Int
  , battingLineStrikeouts :: !Int
  , battingLineLOB :: !Int
  } deriving (Eq, Show, Generic)

makeClassy_ ''Batting
makeClassy_ ''BattingLine

instance Monoid Int where
  mempty  = 0
  mappend = (+)

instance Monoid BattingLine where
  mempty = initialBattingLine "Total"
  mappend = mappenddefault

playerIdL, atBatsL, runsL, hitsL, rbiL, walksL, strikeoutsL, lobL :: ByteString
playerIdL   = "PlayerID"
atBatsL     = "AtBats"
runsL       = "Runs"
hitsL       = "Hits"
rbiL        = "RBI"
walksL      = "Walks"
strikeoutsL = "Strikeouts"
lobL        = "LOB"

toBattingCsv :: Batting -> BL.ByteString
toBattingCsv Batting{..} = encode $ HashMap.elems battingStats

battingCsvHeader :: Header
battingCsvHeader = header [ playerIdL, atBatsL, runsL, hitsL, rbiL, walksL, strikeoutsL, lobL ]

instance ToNamedRecord BattingLine where
  toNamedRecord BattingLine{..} = namedRecord
    [ playerIdL   .= battingLinePlayerId
    , atBatsL     .= battingLineAtBats
    , runsL       .= battingLineRuns
    , hitsL       .= battingLineHits
    , rbiL        .= battingLineRBI
    , walksL      .= battingLineWalks
    , strikeoutsL .= battingLineStrikeouts
    , lobL        .= battingLineLOB
    ]

instance ToRecord BattingLine where
  toRecord BattingLine{..} = record
    [ toField battingLinePlayerId
    , toField battingLineAtBats
    , toField battingLineRuns
    , toField battingLineHits
    , toField battingLineRBI
    , toField battingLineWalks
    , toField battingLineStrikeouts
    , toField battingLineLOB
    ]

initialBatting :: Batting
initialBatting = Batting HashMap.empty initialBattingOrderMap initialBattingOrderMap

initialBattingLine :: Text -> BattingLine
initialBattingLine playerId = BattingLine playerId 0 0 0 0 0 0 0

initialBattingOrderMap :: BattingOrderMap
initialBattingOrderMap = HashMap.fromList $ zip [minBound ..] $ repeat []

updateBattingWithPlay :: PlayEvent -> FrameState -> Batting -> Batting
updateBattingWithPlay pe@PlayEvent{..} gs b = b
  & batting %~ (if isAtBat playEventResult then addAtBatToPlayer playEventPlayerId else id)
  & batting %~ (if isHit playEventResult then addHitToPlayer playEventPlayerId else id)
  & batting %~ (if isWalk playEventResult then addWalkToPlayer playEventPlayerId else id)
  & batting %~ (if isStrikeout playEventResult then addStrikeoutToPlayer playEventPlayerId else id)
  & batting %~ (if isAtBat playEventResult && isOut pe then addLOB playEventPlayerId playEventResult gs else id)
  & batting %~ addRBIToPlayer playEventPlayerId (numRBI playEventResult)
  & batting %~ addRuns playEventPlayerId playEventResult gs

addPlayerToBatting :: HomeOrAway -> Text -> BattingOrderPosition -> Batting -> Batting
addPlayerToBatting homeOrAway player battingPosition bs =
  case bs ^. _battingStats . at player of
    Just _ -> bs
    Nothing ->
      let
        _battingOrder = case homeOrAway of
          Away -> _battingAwayOrder
          Home -> _battingHomeOrder
      in
        bs
        & _battingStats . at player ?~ initialBattingLine player
        & _battingOrder . at battingPosition %~ map (++ [player])

isOut :: PlayEvent -> Bool
isOut PlayEvent{..} = flip any (playActions playEventResult) $ \a -> case a of
  Strikeout _ -> not $ any isBatterAdvancedOnMovement (playMovements playEventResult)
  FieldersChoice _ -> True
  RoutinePlay _ _ -> True
  Error _ -> True
  _ -> False

numNotLeftOnBase :: Play -> Int
numNotLeftOnBase Play{..} =
  length $ filter (\m -> case m of PlayMovement _ HomePlate True -> True; _ -> False) playMovements

addLOB :: Text -> Play -> FrameState -> Batting -> Batting
addLOB playerId pr FrameState{..} score =
  let
    numOB = length $ catMaybes [frameStateRunnerOnFirstId, frameStateRunnerOnSecondId, frameStateRunnerOnThirdId]
    numLOB = numOB - numNotLeftOnBase pr
  in
    addLOBToPlayer playerId numLOB score

addRunForMovement :: Text ->  FrameState -> PlayMovement -> Batting -> Batting
addRunForMovement _ state (PlayMovement startBase HomePlate True) score =
  fromMaybe score $ map (`addRunToPlayer` score) $ runnerOnBase startBase state
addRunForMovement _ _ _ score = score

addRuns :: Text -> Play -> FrameState -> Batting -> Batting
addRuns player Play{..} state score =
  let
    hitRun = flip any playActions $ \a -> case a of
      Hit HomePlate _ -> True
      _ -> False
    scoreWithRun = if hitRun then addRunToPlayer player score else score
  in
    foldr (addRunForMovement player state) scoreWithRun playMovements

addToPlayer
  :: Text
  -> Int
  -> ASetter BattingLine BattingLine Int Int
  -> Batting
  -> Batting
addToPlayer player num stat bs =
  bs & _battingStats . at player %~ map (stat %~ (+num))

addOneToPlayer
  :: Text
  -> ASetter BattingLine BattingLine Int Int
  -> Batting
  -> Batting
addOneToPlayer player stat = addToPlayer player 1 stat

addLOBToPlayer :: Text -> Int -> Batting -> Batting
addLOBToPlayer player lob = addToPlayer player lob _battingLineLOB

addRunToPlayer :: Text -> Batting -> Batting
addRunToPlayer player = addOneToPlayer player _battingLineRuns

addAtBatToPlayer :: Text -> Batting -> Batting
addAtBatToPlayer player = addOneToPlayer player _battingLineAtBats

addHitToPlayer :: Text -> Batting -> Batting
addHitToPlayer player = addOneToPlayer player _battingLineHits

addRBIToPlayer :: Text -> Int -> Batting -> Batting
addRBIToPlayer player rbi = addToPlayer player rbi _battingLineRBI

addWalkToPlayer :: Text -> Batting -> Batting
addWalkToPlayer player = addOneToPlayer player _battingLineWalks

addStrikeoutToPlayer :: Text -> Batting -> Batting
addStrikeoutToPlayer player = addOneToPlayer player _battingLineStrikeouts

prettyPrintBatting :: Batting -> Text
prettyPrintBatting Batting{..} =
  unlines
    [ "------------------------------------"
    , "Home Batters    AB R H RBI BB SO LOB"
    , "------------------------------------"
    , prettyPrintBattingOrderMap battingHomeOrder battingStats
    , "------------------------------------"
    , "Away Batters    AB R H RBI BB SO LOB"
    , "------------------------------------"
    , prettyPrintBattingOrderMap battingAwayOrder battingStats
    ]

prettyPrintBattingOrderMap :: BattingOrderMap -> HashMap Text BattingLine -> Text
prettyPrintBattingOrderMap bom counts =
  let
    (_:hitters) = [(minBound :: BattingOrderPosition) ..]
    battingLines = reverse $ map (counts HashMap.!) $ concatMap (bom HashMap.!) hitters
    battingTotals = mconcat battingLines
    playerLines = unlines $ map (\i ->
        tshow (fromIntegral i :: Integer) <> ": "
        <> prettyPrintBattingLines (reverse $ map (counts HashMap.!) (bom HashMap.! i))
      ) hitters
  in
    playerLines <> prettyPrintBattingTotals battingTotals

prettyPrintBattingTotals :: BattingLine -> Text
prettyPrintBattingTotals bl =
  unlines
    [ "-----------------------------------"
    , prettyPrintBattingLine (set _battingLinePlayerId "Total:  " bl)
    ]

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
