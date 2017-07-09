{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import ClassyPrelude hiding (head)
import Data.List (head)

import Baseball.BoxScore
import Baseball.Game
import Retrosheet.Parser

printGameAndBoxScore :: Game -> IO ()
printGameAndBoxScore g = do
  putStrLn $ prettyPrintGameInfo g
  putStrLn $ prettyPrintBoxScore $ generateBoxScore g

main :: IO ()
main = do
  eventFile <- unpack . head <$> getArgs
  gamesFromFilePath eventFile >>= mapM_ printGameAndBoxScore
