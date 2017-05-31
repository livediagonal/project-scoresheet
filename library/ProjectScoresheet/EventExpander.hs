{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module ProjectScoresheet.EventExpander where

import ClassyPrelude
import Data.Csv
import ProjectScoresheet.BaseballTypes
import ProjectScoresheet.GameState
import ProjectScoresheet.RawTypes
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
    }

processPlayLine :: Game -> RawPlay -> Game
processPlayLine game RawPlay{..} =
  let
    prevState = gameState game
    -- boxScore = addPitchSequenceToBoxScore boxScore rawPlayPitchSequence
  in
    game
      { gameState = prevState
        { gameStateOuts = rawPlayOuts
        , gameStateInning = rawPlayInning
        }
      , gameLastPlay = Just rawPlayResult
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
    }

-- achta001,Achter,A.J.,R,R,ANA,P
-- loadRoster :: Text -> Text -> Text
-- loadRoster team year = 
--   let 
--     fileName = "./data/" <> year <> "eve/" toUpper team <> year <> ".ROS"
--     rosterCsv = BL.readFile fileName
--   in

main :: IO ()
main = do
  csvEvents <- BL.readFile "testgame.txt"
  case (decode NoHeader csvEvents :: Either String (Vector EventFileLine)) of
    Left err -> print err
    Right v -> do
      let gameStates = V.tail $ V.scanl processEvent unstartedGame v
      mapM_ print $ toList gameStates
