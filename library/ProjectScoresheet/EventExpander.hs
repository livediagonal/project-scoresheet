{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module ProjectScoresheet.EventExpander where

import ClassyPrelude
import Data.Csv
import ProjectScoresheet.BaseballTypes
import ProjectScoresheet.GameState
import ProjectScoresheet.BoxScore
import ProjectScoresheet.RawTypes
import ProjectScoresheet.Print
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

processEvent :: Game -> EventFileLine -> Game
processEvent game (InfoLine rawInfo) = processInfoLine game rawInfo
processEvent game (StartLine rawStart) = processStartLine game rawStart
processEvent game (SubLine rawSub) = processSubLine game rawSub
processEvent game (PlayLine rawPlay) = processPlayLine game rawPlay
processEvent game _ = game

processInfoLine :: Game -> RawInfo -> Game
processInfoLine game RawInfo{..} =
  case rawInfoKey of
    "visteam" -> game { gameAwayTeam = Just rawInfoValue }
    "hometeam" -> game { gameHomeTeam = Just rawInfoValue }
    "date" -> game { gameDate = Just rawInfoValue }

    "starttime" -> game { gameStartTime = Just rawInfoValue }
    _ -> game

processStartLine :: Game -> RawStart -> Game
processStartLine game@Game{..} RawStart{..} =
  game
    { gameState = case rawStartPlayerHome of
        Away -> gameState
          { gameStateAwayBattingOrder = addToBattingOrder rawStartPlayer rawStartBattingPosition $ gameStateAwayBattingOrder gameState
          , gameStateAwayFieldingLineup =  addToFieldingLineup rawStartPlayer rawStartFieldingPosition $ gameStateAwayFieldingLineup gameState
          }
        Home -> gameState
          { gameStateHomeBattingOrder = addToBattingOrder rawStartPlayer rawStartBattingPosition $ gameStateHomeBattingOrder gameState
          , gameStateHomeFieldingLineup =  addToFieldingLineup rawStartPlayer rawStartFieldingPosition $ gameStateHomeFieldingLineup gameState
          }
    , gameBoxScore = addPlayerToBoxScore rawStartPlayerHome rawStartPlayer rawStartBattingPosition rawStartFieldingPosition gameBoxScore
    }

processPlayLine :: Game -> RawPlay -> Game
processPlayLine game@Game{..} RawPlay{..} =
  game
    { gameState = gameState
      { gameStateOuts = rawPlayOuts
      , gameStateInning = rawPlayInning
      }
    , gameLastPlay = Just rawPlayResult
    , gameBoxScore = addPlayToBoxScore rawPlayPlayerId rawPlayPitchSequence rawPlayResult gameBoxScore
    }

processSubLine :: Game -> RawSub -> Game
processSubLine game@Game{..} RawSub{..} =
  game
    { gameState = case rawSubPlayerHome of
      Away -> gameState
        { gameStateAwayBattingOrder = addToBattingOrder rawSubPlayer rawSubBattingPosition $ gameStateAwayBattingOrder gameState
        , gameStateAwayFieldingLineup = addToFieldingLineup rawSubPlayer rawSubFieldingPosition $ gameStateAwayFieldingLineup gameState
        }
      Home -> gameState
        { gameStateHomeBattingOrder = addToBattingOrder rawSubPlayer rawSubBattingPosition $ gameStateHomeBattingOrder gameState
        , gameStateHomeFieldingLineup = addToFieldingLineup rawSubPlayer rawSubFieldingPosition $ gameStateHomeFieldingLineup gameState
        }
      , gameBoxScore = addPlayerToBoxScore rawSubPlayerHome rawSubPlayer rawSubBattingPosition rawSubFieldingPosition gameBoxScore
    }

main :: IO ()
main = do
  csvEvents <- BL.readFile "testgame.txt"
  case (decode NoHeader csvEvents :: Either String (Vector EventFileLine)) of
    Left err -> print err
    Right v -> do
      let gameStates = V.tail $ V.scanl processEvent unstartedGame v
      mapM_ (putStrLn . prettyPrintBoxScore . gameBoxScore) $ toList gameStates
