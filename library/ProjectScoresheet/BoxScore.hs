{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module ProjectScoresheet.BoxScore where

import ClassyPrelude
import Control.Lens
import ProjectScoresheet.BaseballTypes
import ProjectScoresheet.EventTypes
import ProjectScoresheet.GameState
import ProjectScoresheet.PlayResult
import qualified Data.HashMap.Strict as HashMap

data InningLine
  = InningLine
  { hits :: !Int
  , runs :: !Int
  , errors :: !Int
  } deriving (Eq, Show)

makeClassy_ ''InningLine

initialInningLine :: InningLine
initialInningLine = InningLine 0 0 0

type BattingOrderMap = HashMap BattingOrderPosition [Text]

initialBattingOrderMap :: BattingOrderMap
initialBattingOrderMap = HashMap.fromList $ zip [minBound ..] $ repeat []

data PitchingLine
  = PitchingLine
  { strikes :: !Int
  } deriving (Eq, Show)

makeClassy_ ''PitchingLine

initialPitchingLine :: PitchingLine
initialPitchingLine = PitchingLine 0

data TeamBoxScore
  = TeamBoxScore
  { innings :: [InningLine]
  } deriving (Eq, Show)

makeClassy_ ''TeamBoxScore

initialTeamBoxScore :: TeamBoxScore
initialTeamBoxScore = TeamBoxScore []

data BoxScoreCounts
  = BoxScoreCounts
  { boxScoreCountsAtBats :: HashMap Text Int
  , boxScoreCountsHits :: HashMap Text Int
  , boxScoreCountsRBI :: HashMap Text Int
  , boxScoreCountsBB :: HashMap Text Int
  , boxScoreCountsStrikeouts :: HashMap Text Int
  } deriving (Eq, Show)

makeClassy_ ''BoxScoreCounts

initialBoxScoreCount :: BoxScoreCounts
initialBoxScoreCount = BoxScoreCounts HashMap.empty HashMap.empty HashMap.empty HashMap.empty HashMap.empty

data BoxScore
  = BoxScore
  { boxScoreStats :: BoxScoreCounts
  -- , boxScorePitchingLines :: HashMap Text PitchingLine
  , boxScoreHomeBattingOrderMap :: BattingOrderMap
  , boxScoreAwayBattingOrderMap :: BattingOrderMap
  } deriving (Eq, Show)

makeClassy_ ''BoxScore

initialBoxScore :: BoxScore
initialBoxScore = BoxScore initialBoxScoreCount initialBattingOrderMap initialBattingOrderMap 

generateBoxScore :: [EventWithContext] -> BoxScore
generateBoxScore events = foldr updateBoxScore initialBoxScore events

updateBoxScore :: EventWithContext -> BoxScore -> BoxScore
updateBoxScore (EventWithContext (StartEventType startEvent) _) = processStartEvent startEvent
updateBoxScore (EventWithContext (SubEventType subEvent) _) = processSubEvent subEvent
updateBoxScore (EventWithContext (PlayEventType playEvent) _) = processPlayEvent playEvent
updateBoxScore _ = id

processInfoEvent :: InfoEvent -> Game -> Game
processInfoEvent InfoEvent{..} = do
  let info = Just infoEventValue
  case infoEventKey of
    "visteam" -> set _gameAwayTeam info
    "hometeam" -> set _gameHomeTeam info
    "date" -> set _gameDate info
    "starttime" -> set _gameStartTime info
    _ -> id

processStartEvent :: StartEvent -> BoxScore -> BoxScore
processStartEvent StartEvent{..} =
  addPlayerToBoxScore startEventPlayerHome startEventPlayer startEventBattingPosition startEventFieldingPosition

processSubEvent :: SubEvent -> BoxScore -> BoxScore
processSubEvent SubEvent{..} =
  addPlayerToBoxScore subEventPlayerHome subEventPlayer subEventBattingPosition subEventFieldingPosition

processPlayEvent :: PlayEvent -> BoxScore -> BoxScore
processPlayEvent PlayEvent{..} =
  over boxScore (addAtBatsToPlayer playEventPlayerId (if isAtBat playEventResult then 1 else 0))
  . over boxScore (addHitsToPlayer playEventPlayerId (if isHit playEventResult then 1 else 0))
  . over boxScore (addRBIToPlayer playEventPlayerId (numRBI playEventResult))
  . over boxScore (addWalkToPlayer playEventPlayerId (isWalk playEventResult))
  . over boxScore (addStrikeoutToPlayer playEventPlayerId (isStrikeout playEventResult))

addAtBatsToPlayer :: Text -> Int -> BoxScore -> BoxScore
addAtBatsToPlayer player numAtBats = over _boxScoreStats (over _boxScoreCountsAtBats (HashMap.insertWith (+) player numAtBats))

addHitsToPlayer :: Text -> Int -> BoxScore -> BoxScore
addHitsToPlayer player numHits = over _boxScoreStats (over _boxScoreCountsHits (HashMap.insertWith (+) player numHits))

addRBIToPlayer :: Text -> Int -> BoxScore -> BoxScore
addRBIToPlayer player rbi = over _boxScoreStats (over _boxScoreCountsRBI (HashMap.insertWith (+) player rbi))

addWalkToPlayer :: Text -> Bool -> BoxScore -> BoxScore
addWalkToPlayer player bb = over _boxScoreStats (over _boxScoreCountsBB (HashMap.insertWith (+) player (if bb then 1 else 0)))

addStrikeoutToPlayer :: Text -> Bool -> BoxScore -> BoxScore
addStrikeoutToPlayer player strikeout = over _boxScoreStats (over _boxScoreCountsStrikeouts (HashMap.insertWith (+) player (if strikeout then 1 else 0)))

addPlayerToBoxScore :: HomeOrAway -> Text -> BattingOrderPosition -> FieldingPositionId -> BoxScore -> BoxScore
addPlayerToBoxScore homeOrAway player battingPosition fieldingPosition =
  let
    addPlayer = addPlayerToBattingOrderMap battingPosition player
  in
    case homeOrAway of
      Away -> over _boxScoreAwayBattingOrderMap addPlayer
      Home -> over _boxScoreHomeBattingOrderMap addPlayer

addPlayerToBattingOrderMap :: BattingOrderPosition -> Text -> BattingOrderMap -> BattingOrderMap
addPlayerToBattingOrderMap position player battingOrderMap = 
  let
    players = battingOrderMap HashMap.! position
  in
    case player `elem` players of
      True -> battingOrderMap
      False -> HashMap.insertWith (++) position [player] battingOrderMap

-- addPlayerToPitching :: Text -> FieldingPositionId -> [PitchingLine] -> [PitchingLine]
-- addPlayerToPitching playerId 1 pitching =
--   pitching ++ [initialPitchingLine]
-- addPlayerToPitching _ _ pitching = pitching
