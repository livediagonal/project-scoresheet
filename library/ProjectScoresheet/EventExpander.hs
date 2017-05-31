{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module ProjectScoresheet.EventExpander where

import ClassyPrelude
import Data.Csv
import ProjectScoresheet.GameState
import ProjectScoresheet.RawTypes
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

prettyPrintLineupSlot :: Maybe LineupSlot -> Text
prettyPrintLineupSlot (Just LineupSlot{..}) = tshow lineupSlotPlayerId <> ", " <> tshow lineupSlotFieldPosition
prettyPrintLineupSlot Nothing = "Player not loaded."

prettyPrintLineup :: Lineup -> Text
prettyPrintLineup Lineup{..} =
  unlines
    [ "1: " <> prettyPrintLineupSlot lineupSlotOne
    , "2: " <> prettyPrintLineupSlot lineupSlotTwo
    , "3: " <> prettyPrintLineupSlot lineupSlotThree
    , "4: " <> prettyPrintLineupSlot lineupSlotFour
    , "5: " <> prettyPrintLineupSlot lineupSlotFive
    , "6: " <> prettyPrintLineupSlot lineupSlotSix
    , "7: " <> prettyPrintLineupSlot lineupSlotSeven
    , "8: " <> prettyPrintLineupSlot lineupSlotEight
    , "9: " <> prettyPrintLineupSlot lineupSlotNine
    ]

prettyPrintGameState :: GameState -> Text
prettyPrintGameState GameState{..} =
  unlines
    [ "Inning: " <> tshow gameStateInning <> ", Outs: " <> tshow gameStateOuts
    , ""
    , "Away: "
    , prettyPrintLineup gameStateAwayLineup
    , "Home: "
    , prettyPrintLineup gameStateHomeLineup
    ]

prettyPrintGame :: Game -> Text
prettyPrintGame Game{..} =
  unlines
    [ tshow (fromMaybe "" gameAwayTeam) <> "@" <> tshow (fromMaybe "" gameHomeTeam)
    , ""
    , prettyPrintGameState gameState
    ]

addToLineup :: LineupSlot -> BattingPosition -> Lineup -> Lineup
addToLineup slot (BattingPosition 1) lineup = lineup { lineupSlotOne = Just slot }
addToLineup slot (BattingPosition 2) lineup = lineup { lineupSlotTwo = Just slot }
addToLineup slot (BattingPosition 3) lineup = lineup { lineupSlotThree = Just slot }
addToLineup slot (BattingPosition 4) lineup = lineup { lineupSlotFour = Just slot }
addToLineup slot (BattingPosition 5) lineup = lineup { lineupSlotFive = Just slot }
addToLineup slot (BattingPosition 6) lineup = lineup { lineupSlotSix = Just slot }
addToLineup slot (BattingPosition 7) lineup = lineup { lineupSlotSeven = Just slot }
addToLineup slot (BattingPosition 8) lineup = lineup { lineupSlotEight = Just slot }
addToLineup slot (BattingPosition 9) lineup = lineup { lineupSlotNine = Just slot }
addToLineup _ _ lineup = lineup

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
    slot = LineupSlot rawStartPlayer $ fieldPositionFromId rawStartFieldingPosition
  in
    game
      { gameState = case rawStartPlayerHome of
          Away -> prevState { gameStateAwayLineup = addToLineup slot rawStartBattingPosition $ gameStateAwayLineup prevState }
          Home -> prevState { gameStateHomeLineup = addToLineup slot rawStartBattingPosition $ gameStateHomeLineup prevState }
      }

processPlayLine :: Game -> RawPlay -> Game
processPlayLine game RawPlay{..} =
  let
    prevState = gameState game
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
    slot = LineupSlot rawSubPlayer $ fieldPositionFromId rawSubFieldingPosition
  in
    game
      { gameState = case rawSubPlayerHome of
        Away -> prevState { gameStateAwayLineup = addToLineup slot rawSubBattingPosition $ gameStateAwayLineup prevState }
        Home -> prevState { gameStateHomeLineup = addToLineup slot rawSubBattingPosition $ gameStateHomeLineup prevState }
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
      mapM_ print $ mapMaybe gameLastPlay $ toList gameStates
