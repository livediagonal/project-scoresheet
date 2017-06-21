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
import ProjectScoresheet.Retrosheet.Events
import ProjectScoresheet.GameState
import ProjectScoresheet.PlayResult
import ProjectScoresheet.PlayResultUtils
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

generateBoxScore :: Game -> BoxScore
generateBoxScore = foldl' (flip updateBoxScore) initialBoxScore . gameEvents

updateBoxScore :: EventWithState -> BoxScore -> BoxScore
updateBoxScore (EventWithState (StartEventType startEvent) _) bs = processStartEvent startEvent bs
updateBoxScore (EventWithState (SubEventType subEvent) _) bs = processSubEvent subEvent bs
updateBoxScore (EventWithState (PlayEventType pe) ctx) bs = processPlayEvent pe ctx bs
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

processPlayEvent :: PlayEvent -> FrameState -> BoxScore -> BoxScore
processPlayEvent pe@PlayEvent{..} gs score = score
  & boxScore %~ (if isAtBat playEventResult then addAtBatToPlayer playEventPlayerId else id)
  & boxScore %~ (if isHit playEventResult then addHitToPlayer playEventPlayerId else id)
  & boxScore %~ (if isWalk playEventResult then addWalkToPlayer playEventPlayerId else id)
  & boxScore %~ (if isStrikeout playEventResult then addStrikeoutToPlayer playEventPlayerId else id)
  & boxScore %~ (if isAtBat playEventResult && isOut pe then addLOB playEventPlayerId playEventResult gs else id)
  & boxScore %~ addRBIToPlayer playEventPlayerId (numRBI playEventResult)
  & boxScore %~ addRuns playEventPlayerId playEventResult gs

isOut :: PlayEvent -> Bool
isOut PlayEvent{..} = case playResultAction playEventResult of
  Outs _ -> True
  _ -> False

numNotLeftOnBase :: PlayResult -> Int
numNotLeftOnBase PlayResult{..} =
  length $ filter (\m -> case m of PlayMovement _ HomePlate True -> True; _ -> False) playResultMovements

addLOB :: Text -> PlayResult -> FrameState -> BoxScore -> BoxScore
addLOB playerId pr FrameState{..} score =
  let
    numOB = length $ catMaybes [frameStateRunnerOnFirstId, frameStateRunnerOnSecondId, frameStateRunnerOnThirdId]
    numLOB = numOB - numNotLeftOnBase pr
  in
    addLOBToPlayer playerId numLOB score

getRunnerOnBase :: Base -> FrameState -> Maybe Text
getRunnerOnBase FirstBase = frameStateRunnerOnFirstId
getRunnerOnBase SecondBase = frameStateRunnerOnSecondId
getRunnerOnBase ThirdBase = frameStateRunnerOnThirdId
getRunnerOnBase _ = const Nothing

addRunForMovement :: Text ->  FrameState -> PlayMovement -> BoxScore -> BoxScore
addRunForMovement _ state (PlayMovement startBase HomePlate True) score =
  fromMaybe score $ map (`addRunToPlayer` score) $ getRunnerOnBase startBase state
addRunForMovement _ _ _ score = score

addRuns :: Text -> PlayResult -> FrameState -> BoxScore -> BoxScore
addRuns player PlayResult{..} state score =
  let
    scoreWithRun = case playResultAction of
      Hit HomePlate _ -> addRunToPlayer player score
      _ -> score
  in
    foldr (addRunForMovement player state) scoreWithRun playResultMovements

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


