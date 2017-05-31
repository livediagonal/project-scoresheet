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

prettyPrintLineupSlot :: Maybe Text -> Text
prettyPrintLineupSlot (Just playerId) = playerId
prettyPrintLineupSlot Nothing = "Player not loaded."

prettyPrintLineup :: BattingOrder -> Text
prettyPrintLineup BattingOrder{..} =
  unlines
    [ "1: " <> prettyPrintLineupSlot battingOrderSpotOnePlayerId
    , "2: " <> prettyPrintLineupSlot battingOrderSpotTwoPlayerId
    , "3: " <> prettyPrintLineupSlot battingOrderSpotThreePlayerId
    , "4: " <> prettyPrintLineupSlot battingOrderSpotFourPlayerId
    , "5: " <> prettyPrintLineupSlot battingOrderSpotFivePlayerId
    , "6: " <> prettyPrintLineupSlot battingOrderSpotSixPlayerId
    , "7: " <> prettyPrintLineupSlot battingOrderSpotSevenPlayerId
    , "8: " <> prettyPrintLineupSlot battingOrderSpotEightPlayerId
    , "9: " <> prettyPrintLineupSlot battingOrderSpotNinePlayerId
    ]

prettyPrintGameState :: GameState -> Text
prettyPrintGameState GameState{..} =
  unlines
    [ "Inning: " <> tshow gameStateInning <> ", Outs: " <> tshow gameStateOuts
    , ""
    , "Away: "
    , prettyPrintLineup gameStateAwayBattingOrder
    , "Home: "
    , prettyPrintLineup gameStateAwayBattingOrder
    ]

prettyPrintGame :: Game -> Text
prettyPrintGame Game{..} =
  unlines
    [ tshow (fromMaybe "" gameAwayTeam) <> "@" <> tshow (fromMaybe "" gameHomeTeam)
    , ""
    , prettyPrintGameState gameState
    ]

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
processStartLine game RawStart{..} =
  let 
    prevState = gameState game
  in
    game
      { gameState = case rawStartPlayerHome of
          Away -> prevState 
            { gameStateAwayBattingOrder = addToBattingOrder rawStartPlayer rawStartBattingPosition $ gameStateAwayBattingOrder prevState 
            , gameStateAwayFieldingLineup =  addToFieldingLineup rawStartPlayer rawStartFieldingPosition $ gameStateAwayFieldingLineup prevState
            }
          Home -> prevState 
            { gameStateHomeBattingOrder = addToBattingOrder rawStartPlayer rawStartBattingPosition $ gameStateHomeBattingOrder prevState 
            , gameStateHomeFieldingLineup =  addToFieldingLineup rawStartPlayer rawStartFieldingPosition $ gameStateHomeFieldingLineup prevState
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
processSubLine game RawSub{..} =
  let
    prevState = gameState game
  in
    game
    { gameState = case rawSubPlayerHome of
        Away -> prevState 
          { gameStateAwayBattingOrder = addToBattingOrder rawSubPlayer rawSubBattingPosition $ gameStateAwayBattingOrder prevState 
          , gameStateAwayFieldingLineup = addToFieldingLineup rawSubPlayer rawSubFieldingPosition $ gameStateAwayFieldingLineup prevState
          }
        Home -> prevState 
          { gameStateHomeBattingOrder = addToBattingOrder rawSubPlayer rawSubBattingPosition $ gameStateHomeBattingOrder prevState 
          , gameStateHomeFieldingLineup = addToFieldingLineup rawSubPlayer rawSubFieldingPosition $ gameStateHomeFieldingLineup prevState
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
