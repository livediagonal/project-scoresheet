{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import ClassyPrelude hiding (head, putStr)
import Data.List (head)
import Data.String.Class (putStr)
import ProjectScoresheet.BoxScore
import ProjectScoresheet.BoxScore.Batting
import ProjectScoresheet.Game

main :: IO ()
main = do
  eventFile <- unpack . head <$> getArgs
  gamesFromFilePath eventFile >>= mapM_ (putStr . toBattingCsv . boxScoreBatting . generateBoxScore)
