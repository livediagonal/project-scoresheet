{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module ProjectScoresheet.EventExpander where

import ClassyPrelude hiding (zipWith)
import Control.Lens (over)
import Data.List (zipWith)
import Data.Csv
import ProjectScoresheet.BoxScore
import ProjectScoresheet.EventTypes
import ProjectScoresheet.GameState
import ProjectScoresheet.PlayResult
import ProjectScoresheet.Print
import qualified Data.ByteString.Lazy as BL

cleanEvent :: Event -> Event
cleanEvent event =
  case event of
    PlayEventType pe@PlayEvent{..} -> PlayEventType $ over _playEventResult saturatePlayMovements pe
    _ -> event

boxScoreFromFile :: String -> IO BoxScore
boxScoreFromFile file = do
  csvEvents <- BL.readFile file
  case (decode NoHeader csvEvents :: Either String (Vector Event)) of
    Left err -> fail err
    Right v -> do
      let
        events = map cleanEvent $ toList v
        gameStates = unstartedGameState : zipWith updateGameState events gameStates
        eventsWithContext = zipWith EventWithContext events gameStates
      pure $ generateBoxScore eventsWithContext

main :: IO ()
main = prettyPrintBoxScore <$> boxScoreFromFile "testgame.txt" >>= putStrLn
