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

data BattingLine
  = BattingLine
  { battingLinePlayerId :: !Text
  , battingLineAtBats :: !Int
  , battingLinePlateAppearances :: !Int
  , battingLineHits :: !Int
  , battingLineRuns :: !Int
  , battingLineSingles :: !Int
  , battingLineDoubles :: !Int
  , battingLineTriples :: !Int
  , battingLineHomeRuns :: !Int
  , battingLineGrandSlams :: !Int
  , battingLineRunsBattingIn :: !Int
  , battingLineTwoOutRunsBattingIn :: !Int
  , battingLineLeftOnBase :: !Int
  , battingLineWalks :: !Int
  , battingLineIntentionalWalks :: !Int
  , battingLineStrikeOuts :: !Int
  , battingLineGroundIntoDoublePlays :: !Int
  , battingLineSacrificeBunts :: !Int
  , battingLineSacrificeFlys :: !Int
  , battingLineHitByPitches :: !Int
  , battingLineStolenBases :: !Int
  , battingLineCaughtStealing :: !Int
  , battingLineReachedOnErrors :: !Int
  } deriving (Eq, Show)

initialBattingLine :: Text -> BattingLine
initialBattingLine playerId = BattingLine playerId 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

type BattingLines = HashMap BattingOrderPosition [BattingLine]

initialBattingLines :: BattingLines
initialBattingLines = HashMap.fromList $ zip [minBound ..] $ repeat []

data PitchingLine
  = PitchingLine
  { playerId :: !Text
  , strikes :: !Int
  } deriving (Eq, Show)

makeClassy_ ''PitchingLine

initialPitchingLine :: Text -> PitchingLine
initialPitchingLine playerId = PitchingLine playerId 0

data TeamBoxScore
  = TeamBoxScore
  { innings :: [InningLine]
  , batting :: BattingLines
  , pitching :: [PitchingLine]
  } deriving (Eq, Show)

makeClassy_ ''TeamBoxScore

initialTeamBoxScore :: TeamBoxScore
initialTeamBoxScore = TeamBoxScore [] initialBattingLines []

data BoxScoreCounts
  = BoxScoreCounts
  { boxScoreCountsHits :: HashMap Text Int
  , boxScoreCountsRBI :: HashMap Text Int
  } deriving (Eq, Show)

makeClassy_ ''BoxScoreCounts

initialBoxScoreCount :: BoxScoreCounts
initialBoxScoreCount = BoxScoreCounts HashMap.empty HashMap.empty

data BoxScore
  = BoxScore
  { boxScoreAway :: TeamBoxScore
  , boxScoreHome :: TeamBoxScore
  , boxScoreStats :: BoxScoreCounts
  } deriving (Eq, Show)

makeClassy_ ''BoxScore

initialBoxScore :: BoxScore
initialBoxScore = BoxScore initialTeamBoxScore initialTeamBoxScore initialBoxScoreCount

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
  over boxScore (addHitsToPlayer playEventPlayerId (if isHit playEventResult then 1 else 0))
  . over boxScore (addRBIToPlayer playEventPlayerId (numRBI playEventResult))

addHitsToPlayer :: Text -> Int -> BoxScore -> BoxScore
addHitsToPlayer player numHits =
  over _boxScoreStats (over _boxScoreCountsHits (HashMap.insertWith (+) player numHits))

addRBIToPlayer :: Text -> Int -> BoxScore -> BoxScore
addRBIToPlayer player rbi = over _boxScoreStats (over _boxScoreCountsRBI (HashMap.insertWith (+) player rbi))

addPlayerToBoxScore :: HomeOrAway -> Text -> BattingOrderPosition -> FieldingPositionId -> BoxScore -> BoxScore
addPlayerToBoxScore homeOrAway playerId battingPosition fieldingPosition = do
  let addPlayer = addPlayerToTeamBoxScore playerId battingPosition fieldingPosition
  case homeOrAway of
    Away -> over _boxScoreAway addPlayer
    Home -> over _boxScoreHome addPlayer

addPlayerToTeamBoxScore :: Text -> BattingOrderPosition -> FieldingPositionId -> TeamBoxScore -> TeamBoxScore
addPlayerToTeamBoxScore playerId battingLineId fieldingPosition tbs@TeamBoxScore{..} =
    tbs
    { batting = addPlayerToBatting playerId battingLineId batting
    , pitching = addPlayerToPitching playerId fieldingPosition pitching
    }

addPlayerToBatting :: Text -> BattingOrderPosition -> BattingLines -> BattingLines
addPlayerToBatting _ 0 battingLines = battingLines
addPlayerToBatting playerId battingLineId battingLines =
  let
    initialPlayerBattingLine = initialBattingLine playerId
  in
    case HashMap.lookup battingLineId battingLines of
      Nothing -> HashMap.insert battingLineId [initialPlayerBattingLine] battingLines
      Just battingLineList ->
        case battingLineContains playerId battingLineList of
          True -> battingLines
          False -> HashMap.insert battingLineId (battingLineList ++ [initialPlayerBattingLine]) battingLines

battingLineContains :: Text -> [BattingLine] -> Bool
battingLineContains playerId = any (\bl -> battingLinePlayerId bl == playerId)

addPlayerToPitching :: Text -> FieldingPositionId -> [PitchingLine] -> [PitchingLine]
addPlayerToPitching playerId 1 pitching =
  pitching ++ [initialPitchingLine playerId]
addPlayerToPitching _ _ pitching = pitching

findBattingOrderPosition :: Text -> BattingOrder -> Maybe BattingOrderPosition
findBattingOrderPosition playerId battingOrder =
  map fst $ find (\x -> snd x == playerId) $ HashMap.toList battingOrder
