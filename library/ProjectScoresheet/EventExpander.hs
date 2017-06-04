{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module ProjectScoresheet.EventExpander where

import ClassyPrelude
import Control.Lens
import Data.Csv
import ProjectScoresheet.BaseballTypes
import ProjectScoresheet.GameState
import ProjectScoresheet.BoxScore
import ProjectScoresheet.RawTypes
import ProjectScoresheet.Print
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

processEvent :: EventFileLine -> Game -> Game
processEvent (InfoLine rawInfo) = processInfoLine rawInfo
processEvent (StartLine rawStart) = processStartLine rawStart
processEvent (SubLine rawSub) = processSubLine rawSub
processEvent (PlayLine rawPlay) = processPlayLine rawPlay
processEvent _ = id

processInfoLine :: RawInfo -> Game -> Game
processInfoLine RawInfo{..} = do
  let info = Just rawInfoValue
  case rawInfoKey of
    "visteam" -> set _gameAwayTeam info
    "hometeam" -> set _gameHomeTeam info
    "date" -> set _gameDate info
    "starttime" -> set _gameStartTime info
    _ -> id

updateOrders :: HomeOrAway -> Text -> BattingOrderPosition -> FieldingPositionId -> GameState -> GameState
updateOrders hoa player battingPosition fieldingPosition =
  case hoa of
    Away ->
      over _gameStateAwayBattingOrder (addToBattingOrder player battingPosition)
      . over _gameStateAwayFieldingLineup (addToFieldingLineup player fieldingPosition)
    Home ->
      over _gameStateHomeBattingOrder (addToBattingOrder player battingPosition)
      . over _gameStateHomeFieldingLineup (addToFieldingLineup player fieldingPosition)

processStartLine :: RawStart -> Game -> Game
processStartLine RawStart{..} =
  over _gameGameState (updateOrders rawStartPlayerHome rawStartPlayer rawStartBattingPosition rawStartFieldingPosition)
  . over _gameBoxScore (addPlayerToBoxScore rawStartPlayerHome rawStartPlayer rawStartBattingPosition rawStartFieldingPosition)

processPlayLine :: RawPlay -> Game -> Game
processPlayLine RawPlay{..} =
  set _gameLastPlay (Just rawPlayResult)
  . over _gameGameState (set _gameStateOuts rawPlayOuts . set _gameStateInning rawPlayInning)
  . over _gameBoxScore (addPlayToBoxScore rawPlayPlayerId rawPlayPitchSequence rawPlayResult)

processSubLine :: RawSub -> Game -> Game
processSubLine RawSub{..} =
  over _gameGameState (updateOrders rawSubPlayerHome rawSubPlayer rawSubBattingPosition rawSubFieldingPosition)
  . over _gameBoxScore (addPlayerToBoxScore rawSubPlayerHome rawSubPlayer rawSubBattingPosition rawSubFieldingPosition)

main :: IO ()
main = do
  csvEvents <- BL.readFile "testgame.txt"
  case (decode NoHeader csvEvents :: Either String (Vector EventFileLine)) of
    Left err -> print err
    Right v -> do
      let gameStates = V.tail $ V.scanl (flip processEvent) unstartedGame v
      putStrLn $ prettyPrintBoxScore $ gameBoxScore $ V.last gameStates
