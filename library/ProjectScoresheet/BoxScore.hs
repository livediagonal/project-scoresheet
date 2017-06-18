{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module ProjectScoresheet.BoxScore where

import ClassyPrelude
import Control.Lens
import Data.Csv
import ProjectScoresheet.BaseballTypes
import ProjectScoresheet.EventTypes
import ProjectScoresheet.GameState
import ProjectScoresheet.PlateAppearance
import ProjectScoresheet.PlayResult
import qualified Data.ByteString.Lazy as BL
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
  } deriving (Eq, Show)

makeClassy_ ''BattingLine

emptyBattingLine :: Text -> BattingLine
emptyBattingLine playerId = BattingLine playerId 0 0 0 0 0 0 0

data BoxScore
  = BoxScore
  { boxScoreStats :: !(HashMap Text BattingLine)
  , boxScoreHomeBattingOrderMap :: !BattingOrderMap
  , boxScoreAwayBattingOrderMap :: !BattingOrderMap
  } deriving (Eq, Show)

makeClassy_ ''BoxScore

initialBoxScore :: BoxScore
initialBoxScore = BoxScore HashMap.empty initialBattingOrderMap initialBattingOrderMap

generateBoxScores :: [EventWithContext] -> [BoxScore]
generateBoxScores events = reverse $ foldl' (flip updateBoxScore) [] events

updateBoxScore :: EventWithContext -> [BoxScore] -> [BoxScore]
updateBoxScore (EventWithContext (IdEventType _) _) bss = initialBoxScore : bss
updateBoxScore (EventWithContext (StartEventType startEvent) _) (bs:rest) = processStartEvent startEvent bs : rest
updateBoxScore (EventWithContext (SubEventType subEvent) _) (bs:rest) = processSubEvent subEvent bs : rest
updateBoxScore (EventWithContext (PlayEventType pe) ctx) (bs:rest) = processPlateAppearance (convertEventToPlateAppearance pe) ctx bs : rest
updateBoxScore _ bss = bss

processInfoEvent :: InfoEvent -> Game -> Game
processInfoEvent InfoEvent{..} = do
  let info = Just infoEventValue
  case infoEventKey of
    "visteam" -> _gameAwayTeam .~ info
    "hometeam" -> _gameHomeTeam .~ info
    "date" -> _gameDate .~ info
    "starttime" -> _gameStartTime .~ info
    _ -> id

processStartEvent :: StartEvent -> BoxScore -> BoxScore
processStartEvent StartEvent{..} =
  addPlayerToBoxScore startEventPlayerHome startEventPlayer startEventBattingPosition startEventFieldingPosition

processSubEvent :: SubEvent -> BoxScore -> BoxScore
processSubEvent SubEvent{..} =
  addPlayerToBoxScore subEventPlayerHome subEventPlayer subEventBattingPosition subEventFieldingPosition

processPlateAppearance :: PlateAppearance -> GameState -> BoxScore -> BoxScore
processPlateAppearance pa@PlateAppearance{..} gs score = score
  & boxScore %~ (if isAtBat pa then addAtBatToPlayer plateAppearanceBatterId else id)
  & boxScore %~ (if isHit pa then addHitToPlayer plateAppearanceBatterId else id)
  & boxScore %~ (if isWalk pa then addWalkToPlayer plateAppearanceBatterId else id)
  & boxScore %~ (if isStrikeout pa then addStrikeoutToPlayer plateAppearanceBatterId else id)
  & boxScore %~ (if isAtBat pa && isOut pa then addLOB plateAppearanceBatterId pa gs else id)
  & boxScore %~ addRBIToPlayer plateAppearanceBatterId (numRBI pa)
  & boxScore %~ addRuns pa gs

isOut :: PlateAppearance -> Bool
isOut PlateAppearance{..} = case plateAppearanceAction of
  Outs _ -> True
  _ -> False

numNotLeftOnBase :: PlateAppearance -> Int
numNotLeftOnBase PlateAppearance{..} =
  length $ filter (\m -> case m of PlayMovement _ HomePlate True -> True; _ -> False) plateAppearanceMovements

addLOB :: Text -> PlateAppearance -> GameState -> BoxScore -> BoxScore
addLOB playerId pa GameState{..} score =
  let
    numOB = length $ catMaybes [gameStateRunnerOnFirstId, gameStateRunnerOnSecondId, gameStateRunnerOnThirdId]
    numLOB = numOB - numNotLeftOnBase pa
  in
    addLOBToPlayer playerId numLOB score

getRunnerOnBase :: Base -> GameState -> Maybe Text
getRunnerOnBase FirstBase = gameStateRunnerOnFirstId
getRunnerOnBase SecondBase = gameStateRunnerOnSecondId
getRunnerOnBase ThirdBase = gameStateRunnerOnThirdId
getRunnerOnBase _ = const Nothing

addRunForMovement :: Text ->  GameState -> PlayMovement -> BoxScore -> BoxScore
addRunForMovement _ state (PlayMovement startBase HomePlate True) score =
  fromMaybe score $ map (`addRunToPlayer` score) $ getRunnerOnBase startBase state
addRunForMovement _ _ _ score = score

addRuns :: PlateAppearance -> GameState -> BoxScore -> BoxScore
addRuns PlateAppearance{..} state score =
  let
    scoreWithRun = case plateAppearanceAction of
      Hit HomePlate _ -> addRunToPlayer plateAppearanceBatterId score
      _ -> score
  in
    foldr (addRunForMovement plateAppearanceBatterId state) scoreWithRun plateAppearanceMovements

addToPlayer
  :: Text
  -> Int
  -> ASetter BattingLine BattingLine Int Int
  -> BoxScore
  -> BoxScore
addToPlayer player num stat bs =
  bs & _boxScoreStats . at player %~ map (stat %~ (+num))

addOneToPlayer
  :: Text
  -> ASetter BattingLine BattingLine Int Int
  -> BoxScore
  -> BoxScore
addOneToPlayer player stat = addToPlayer player 1 stat

addLOBToPlayer :: Text -> Int -> BoxScore -> BoxScore
addLOBToPlayer player lob = addToPlayer player lob _battingLineLOB

addRunToPlayer :: Text -> BoxScore -> BoxScore
addRunToPlayer player = addOneToPlayer player _battingLineRuns

addAtBatToPlayer :: Text -> BoxScore -> BoxScore
addAtBatToPlayer player = addOneToPlayer player _battingLineAtBats

addHitToPlayer :: Text -> BoxScore -> BoxScore
addHitToPlayer player = addOneToPlayer player _battingLineHits

addRBIToPlayer :: Text -> Int -> BoxScore -> BoxScore
addRBIToPlayer player rbi = addToPlayer player rbi _battingLineRBI

addWalkToPlayer :: Text -> BoxScore -> BoxScore
addWalkToPlayer player = addOneToPlayer player _battingLineWalks

addStrikeoutToPlayer :: Text -> BoxScore -> BoxScore
addStrikeoutToPlayer player = addOneToPlayer player _battingLineStrikeouts

addPlayerToBoxScore :: HomeOrAway -> Text -> BattingOrderPosition -> FieldingPositionId -> BoxScore -> BoxScore
addPlayerToBoxScore homeOrAway player battingPosition _ bs =
  case bs ^. _boxScoreStats . at player of
    Just _ -> bs
    Nothing ->
      let
        _boxScoreBattingOrder = case homeOrAway of
          Away -> _boxScoreAwayBattingOrderMap
          Home -> _boxScoreHomeBattingOrderMap
      in
        bs
        & _boxScoreStats . at player ?~ emptyBattingLine player
        & _boxScoreBattingOrder . at battingPosition %~ map (++ [player])

-- addPlayerToPitching :: Text -> FieldingPositionId -> [PitchingLine] -> [PitchingLine]
-- addPlayerToPitching playerId 1 pitching =
--   pitching ++ [initialPitchingLine]
-- addPlayerToPitching _ _ pitching = pitching

boxScoresFromFile :: String -> IO [BoxScore]
boxScoresFromFile file = do
  csvEvents <- BL.readFile file
  case (decode NoHeader csvEvents :: Either String (Vector Event)) of
    Left err -> fail err
    Right v -> do
      let
        events = toList v
        gameStates = unstartedGameState : zipWith updateGameState events gameStates
        eventsWithContext = zipWith EventWithContext events gameStates
      pure $ generateBoxScores eventsWithContext


