{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import ClassyPrelude hiding (head, putStr)
import Data.List (head)
import Data.String.Class (putStr)

import Baseball.BoxScore
import Baseball.BoxScore.Batting
import Baseball.Game

main :: IO ()
main = do
  eventFile <- unpack . head <$> getArgs
  gamesFromFilePath eventFile >>= mapM_ (putStr . toBattingCsv . teamBoxScoreBatting . boxScoreHomeTeam  . generateBoxScore)
