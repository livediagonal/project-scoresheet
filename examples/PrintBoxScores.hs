{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import ClassyPrelude hiding (head)
import Data.List (head)

import ProjectScoresheet.BoxScore
import ProjectScoresheet.Game

main :: IO ()
main = do
  eventFile <- unpack . head <$> getArgs
  gamesFromFilePath eventFile >>= mapM_ (putStrLn . prettyPrintBoxScore . generateBoxScore)
