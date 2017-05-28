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
  unlines $ [ "1: " <> prettyPrintLineupSlot lineupSlotOne 
            , "2: " <> prettyPrintLineupSlot lineupSlotTwo
            , "3: " <> prettyPrintLineupSlot lineupSlotThree
            , "4: " <> prettyPrintLineupSlot lineupSlotFour
            , "5: " <> prettyPrintLineupSlot lineupSlotFive
            , "6: " <> prettyPrintLineupSlot lineupSlotSix
            , "7: " <> prettyPrintLineupSlot lineupSlotSeven
            , "8: " <> prettyPrintLineupSlot lineupSlotEight
            , "9: " <> prettyPrintLineupSlot lineupSlotNine ]


prettyPrintGameState :: GameState -> Text
prettyPrintGameState GameState{..} =
    unlines $ [ "Inning: " <> tshow gameStateInning <> ", Outs: " <> tshow gameStateOuts,
                "",
                "Away: ",
                prettyPrintLineup gameStateAwayLineup,
                "Home: ",
                prettyPrintLineup gameStateHomeLineup ]

unstartedGame :: GameState
unstartedGame = GameState 0 0 False emptyLineup emptyLineup Nothing Nothing Nothing Nothing Nothing

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

processEvent :: GameState -> EventFileLine -> GameState
processEvent prevState (StartLine RawStart{..}) =
  let
    slot = LineupSlot rawStartPlayer $ parseFieldingPosition rawStartFieldingPosition
  in
    case rawStartPlayerHome of
      0 -> prevState { gameStateAwayLineup = addToLineup slot rawStartBattingPosition $ gameStateAwayLineup prevState }
      1 -> prevState { gameStateHomeLineup = addToLineup slot rawStartBattingPosition $ gameStateHomeLineup prevState }
processEvent prevEvent (PlayLine RawPlay{..}) =
  let
    isNewInning = rawPlayInning /= gameStateInning prevEvent
  in
    prevEvent
     { gameStateOuts = rawPlayOuts
     , gameStateInning = rawPlayInning
     }
processEvent prevEvent _ = prevEvent

main :: IO ()
main = do
  csvEvents <- BL.readFile "testgame.txt"
  case (decode NoHeader csvEvents :: Either String (Vector EventFileLine)) of
    Left err -> print err
    Right v -> V.mapM_ (putStrLn . prettyPrintGameState) $ V.tail $ V.scanl processEvent unstartedGame v
