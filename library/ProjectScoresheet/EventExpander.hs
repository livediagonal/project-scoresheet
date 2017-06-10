{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ProjectScoresheet.EventExpander where

import ClassyPrelude hiding (zipWith)
import Data.List (zipWith)
import Data.Csv
import ProjectScoresheet.BoxScore
import ProjectScoresheet.EventTypes
import ProjectScoresheet.GameState
import ProjectScoresheet.PlayResult
import ProjectScoresheet.Print
import qualified Data.ByteString.Lazy as BL

boxScoreFromFile :: String -> IO BoxScore
boxScoreFromFile file = do
  csvEvents <- BL.readFile file
  case (decode NoHeader csvEvents :: Either String (Vector Event)) of
    Left err -> fail err
    Right v -> do
      let
        events = toList v
        gameStates = unstartedGameState : zipWith updateGameState events gameStates
        eventsWithContext = zipWith EventWithContext events gameStates
      --traverse (\event -> case event of (PlayEventType playEvent) -> print $ playResultMovements $ playEventResult playEvent; _ -> print "no play") events
      traverse (\(EventWithContext event state) -> do
        print event
        print $ gameStateOuts state
        print $ gameStateRunnerOnFirstId state
        print $ gameStateRunnerOnSecondId state
        print $ gameStateRunnerOnThirdId state
        ) eventsWithContext
      pure $ generateBoxScore eventsWithContext

main :: IO ()
main = prettyPrintBoxScore <$> boxScoreFromFile "testgame.txt" >>= putStrLn
