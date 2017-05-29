{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module ProjectScoresheet.EventExpander where

import ClassyPrelude
import Data.Csv
import Data.Maybe
import ProjectScoresheet.GameState
import ProjectScoresheet.RawTypes
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

prettyPrintLineupSlot :: Maybe LineupSlot -> Text
prettyPrintLineupSlot (Just LineupSlot{..}) = tshow lineupSlotPlayerId <> ", " <> tshow lineupSlotFieldPosition
prettyPrintLineupSlot Nothing = "Player not loaded."

prettyPrintLineup :: Lineup -> Text
prettyPrintLineup Lineup{..} =
  unlines $ 
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
  unlines $ 
    [ "Inning: " <> tshow gameStateInning <> ", Outs: " <> tshow gameStateOuts
    , ""
    , "Away: "
    , prettyPrintLineup gameStateAwayLineup
    , "Home: "
    , prettyPrintLineup gameStateHomeLineup 
    ]

prettyPrintGame :: Game -> Text
prettyPrintGame Game{..} =
  unlines $ 
    [ tshow (fromMaybe "" gameAwayTeam) <> "@" <> tshow (fromMaybe "" gameHomeTeam)
    , ""
    , prettyPrintGameState gameState 
    ]

-- todo improve type sig w/ finite
parseFieldingPosition :: Int -> FieldPosition
parseFieldingPosition 1 = Pitcher
parseFieldingPosition 2 = Catcher
parseFieldingPosition 3 = FirstBaseman
parseFieldingPosition 4 = SecondBaseman
parseFieldingPosition 5 = ThirdBaseman
parseFieldingPosition 6 = ShortStop
parseFieldingPosition 7 = LeftFielder
parseFieldingPosition 8 = CenterFielder
parseFieldingPosition 9 = RightFielder
parseFieldingPosition 10 = DesignatedHitter
parseFieldingPosition 11 = PinchHitter
parseFieldingPosition 12 = PinchRunner

addToLineup :: LineupSlot -> Int -> Lineup -> Lineup
addToLineup _ 0 lineup = lineup
addToLineup slot 1 lineup = lineup { lineupSlotOne = Just slot }
addToLineup slot 2 lineup = lineup { lineupSlotTwo = Just slot }
addToLineup slot 3 lineup = lineup { lineupSlotThree = Just slot }
addToLineup slot 4 lineup = lineup { lineupSlotFour = Just slot }
addToLineup slot 5 lineup = lineup { lineupSlotFive = Just slot }
addToLineup slot 6 lineup = lineup { lineupSlotSix = Just slot }
addToLineup slot 7 lineup = lineup { lineupSlotSeven = Just slot }
addToLineup slot 8 lineup = lineup { lineupSlotEight = Just slot }
addToLineup slot 9 lineup = lineup { lineupSlotNine = Just slot }


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
    slot = LineupSlot rawStartPlayer $ parseFieldingPosition rawStartFieldingPosition
  in
    game 
      { gameState = case rawStartPlayerHome of
          0 -> prevState { gameStateAwayLineup = addToLineup slot rawStartBattingPosition $ gameStateAwayLineup prevState }
          1 -> prevState { gameStateHomeLineup = addToLineup slot rawStartBattingPosition $ gameStateHomeLineup prevState } 
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
      }

processSubLine :: Game -> RawSub -> Game
processSubLine game RawSub{..} =
  let
    prevState = gameState game
    slot = LineupSlot rawSubPlayer $ parseFieldingPosition rawSubFieldingPosition
  in
    game 
      { gameState = case rawSubPlayerHome of
        0 -> prevState { gameStateAwayLineup = addToLineup slot rawSubBattingPosition $ gameStateAwayLineup prevState }
        1 -> prevState { gameStateHomeLineup = addToLineup slot rawSubBattingPosition $ gameStateHomeLineup prevState } 
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
    Right v -> V.mapM_ (putStrLn . prettyPrintGame) $ V.tail $ V.scanl processEvent unstartedGame v
