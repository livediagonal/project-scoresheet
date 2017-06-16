{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ProjectScoresheet.Main where

import ClassyPrelude hiding (head)
import Data.List (head)
import ProjectScoresheet.BoxScore
import ProjectScoresheet.Roster
import ProjectScoresheet.Print

main :: IO ()
main = do
  eventFile <- unpack . head <$> getArgs
  boxScoresFromFile eventFile >>= mapM_ (putStrLn . prettyPrintBoxScore)
