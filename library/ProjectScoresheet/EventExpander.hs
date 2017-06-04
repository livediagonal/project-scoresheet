{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ProjectScoresheet.EventExpander where

import ClassyPrelude
import Data.Csv
import ProjectScoresheet.BoxScore
import ProjectScoresheet.EventTypes
import ProjectScoresheet.GameState
import ProjectScoresheet.Print
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

addEventContext :: Event -> EventWithContext -> EventWithContext
addEventContext event (EventWithContext _ context) = EventWithContext event context

main :: IO ()
main = do
  csvEvents <- BL.readFile "testgame.txt"
  case (decode NoHeader csvEvents :: Either String (Vector Event)) of
    Left err -> print err
    Right v ->
      putStrLn $ prettyPrintBoxScore $ generateBoxScore $ toList $ V.scanl (flip addEventContext) initialContext  v
