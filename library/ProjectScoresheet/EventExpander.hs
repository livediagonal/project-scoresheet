{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ProjectScoresheet.EventExpander where

import ClassyPrelude hiding (zipWith)
import Data.List (zipWith)
import Data.Csv
import ProjectScoresheet.BoxScore
import ProjectScoresheet.EventTypes
import ProjectScoresheet.GameState
import ProjectScoresheet.Print
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
  csvEvents <- BL.readFile "testgame.txt"
  case (decode NoHeader csvEvents :: Either String (Vector Event)) of
    Left err -> print err
    Right v ->
      let
        events = toList v
        gameStates = unstartedGameState : zipWith updateGameState events gameStates
        eventsWithContext = zipWith EventWithContext events gameStates
      in
        putStrLn $ prettyPrintBoxScore $ generateBoxScore $ toList eventsWithContext
