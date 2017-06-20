{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module ProjectScoresheet.Main where

import ClassyPrelude hiding (head)
import Data.List (head)
import ProjectScoresheet.EventTypes
import ProjectScoresheet.GameState
import ProjectScoresheet.Print
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
  eventFile <- unpack . head <$> getArgs
  game <- gameFromFilePath eventFile
  gameFromFilePath eventFile >>= mapM_ print

gameFromFilePath :: String -> IO [Game]
gameFromFilePath file = do
  csvEvents <- BL.readFile file
  case (decode NoHeader csvEvents :: Either String (Vector Event)) of
    Left err -> fail err
    Right v -> do
      let
        events = toList v
        frameStates = unstartedFrameState : zipWith updateFrameState events frameStates
        eventsWithState = zipWith eventWithState events frameStates
      pure $ generateGames eventsWithState

generateGames :: [EventWithState] -> [Game]
generateGames events = reverse $ foldl' (flip updateBoxScore) [] events

updateGame :: EventWithState -> [Game] -> [Game]
updateGame (EventWithState (IdEventType _) _) gss = baseGame : gss
updateGame event (gs:rest) = (addEventToGame event gs) : rest

addEventToGame :: EventWithState -> Game -> Game
addEventToGame event g@Game{..} = g { gameEvents = gameEvents ++ [event] }