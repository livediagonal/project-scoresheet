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
  , boxScoreCountsRuns :: HashMap Text Int
  , boxScoreCountsBB :: HashMap Text Int
  , boxScoreCountsStrikeouts :: HashMap Text Int
  , boxScoreCountsLOB :: HashMap Text Int
  } deriving (Eq, Show)

makeClassy_ ''BoxScoreCounts

initialBoxScoreCount :: BoxScoreCounts
initialBoxScoreCount = BoxScoreCounts HashMap.empty HashMap.empty HashMap.empty HashMap.empty HashMap.empty HashMap.empty HashMap.empty

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
updateBoxScore (EventWithContext (PlayEventType playEvent) ctx) = processPlayEvent playEvent ctx
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

processPlayEvent :: PlayEvent -> GameState -> BoxScore -> BoxScore
processPlayEvent PlayEvent{..} gameState score =
  let
    scoreWithAtBats = if isAtBat playEventResult then addAtBatToPlayer playEventPlayerId score else score
    scoreWithHits = if isHit playEventResult then addHitToPlayer playEventPlayerId scoreWithAtBats else scoreWithAtBats
    scoreWithRBI = addRBIToPlayer playEventPlayerId (numRBI playEventResult) scoreWithHits
    scoreWithWalks = addWalkToPlayer playEventPlayerId (isWalk playEventResult) scoreWithRBI
    scoreWithStrikeouts = addStrikeoutToPlayer playEventPlayerId (isStrikeout playEventResult) scoreWithWalks
    scoreWithRuns = addRuns playEventPlayerId playEventResult gameState scoreWithStrikeouts
  in
    if isAtBat playEventResult && isOut playEventResult then addLOB playEventPlayerId playEventResult gameState scoreWithRuns else scoreWithRuns

isOut :: PlayResult -> Bool
isOut result = case playResultAction result of
  Outs _ -> True
  _ -> False

numNotLeftOnBase :: PlayResult -> Int
numNotLeftOnBase (PlayResult _ _ movements) =
  let
    numScored = length $ filter (\m -> case m of PlayMovement _ HomePlate True -> True; _ -> False) movements
  in
    numScored

addLOB :: Text -> PlayResult -> GameState -> BoxScore -> BoxScore
addLOB playerId playResult GameState{..} score =
  let
    numOB = length $ catMaybes [gameStateRunnerOnFirstId, gameStateRunnerOnSecondId, gameStateRunnerOnThirdId]
    numLOB = numOB - numNotLeftOnBase playResult
  in
    addLOBToPlayer playerId numLOB score

getRunnerOnBase :: Base -> GameState -> Maybe Text
getRunnerOnBase FirstBase GameState{..} = gameStateRunnerOnFirstId
getRunnerOnBase SecondBase GameState{..} = gameStateRunnerOnSecondId
getRunnerOnBase ThirdBase GameState{..} = gameStateRunnerOnThirdId
getRunnerOnBase _ _ = Nothing

addRunForMovement :: Text ->  GameState -> PlayMovement -> BoxScore -> BoxScore
addRunForMovement _ state (PlayMovement startBase HomePlate True) score =
  case getRunnerOnBase startBase state of
    Just runnerId -> addRunToPlayer runnerId score
    Nothing -> score
addRunForMovement _ _ _ score = score

addRuns :: Text -> PlayResult -> GameState -> BoxScore -> BoxScore
addRuns batterId (PlayResult action _ movements) state score =
  let
    scoreWithRun = case action of
      Hit HomePlate _ -> addRunToPlayer batterId score
      _ -> score
  in
    foldr (addRunForMovement batterId state) scoreWithRun movements

addLOBToPlayer :: Text -> Int -> BoxScore -> BoxScore
addLOBToPlayer player numLOB = over _boxScoreStats (over _boxScoreCountsLOB (HashMap.insertWith (+) player numLOB))

addRunToPlayer :: Text -> BoxScore -> BoxScore
addRunToPlayer player = over _boxScoreStats (over _boxScoreCountsRuns (HashMap.insertWith (+) player 1))

addAtBatToPlayer :: Text -> BoxScore -> BoxScore
addAtBatToPlayer player = over _boxScoreStats (over _boxScoreCountsAtBats (HashMap.insertWith (+) player 1))

addHitToPlayer :: Text -> BoxScore -> BoxScore
addHitToPlayer player = over _boxScoreStats (over _boxScoreCountsHits (HashMap.insertWith (+) player 1))

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
