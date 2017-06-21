{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module ProjectScoresheet.Main where

import ClassyPrelude hiding (head)
import Data.List (head)
import Data.Csv
import ProjectScoresheet.EventTypes
import ProjectScoresheet.GameState
import ProjectScoresheet.Print
import ProjectScoresheet.BoxScore
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
  eventFile <- unpack . head <$> getArgs
  gameFromFilePath eventFile >>= mapM_ (putStrLn . prettyPrintBoxScore . generateBoxScore . gameEvents)

gameFromFilePath :: String -> IO [Game]
gameFromFilePath file = do
  csvEvents <- BL.readFile file
  case (decode NoHeader csvEvents :: Either String (Vector Event)) of
    Left err -> fail err
    Right v -> do
      let
        events = toList v
        frameStates = initialFrameState : zipWith updateFrameState events frameStates
        eventsWithState = zipWith EventWithState events frameStates
      pure $ generateGames eventsWithState

generateGames :: [EventWithState] -> [Game]
generateGames events = reverse $ foldl' (flip updateGame) [] events

updateGame :: EventWithState -> [Game] -> [Game]
updateGame (EventWithState (IdEventType _) _) gss = initialGame : gss
updateGame event (gs:rest) = (addEventToGame event gs) : rest

addEventToGame :: EventWithState -> Game -> Game
addEventToGame event g@Game{..} = g { gameEvents = gameEvents ++ [event] }