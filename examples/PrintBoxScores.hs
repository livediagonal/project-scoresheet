{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import ClassyPrelude hiding (head)
import Data.List (head)

import Baseball.BoxScore
import Baseball.Game

printGameAndBoxScore :: Game -> IO ()
printGameAndBoxScore game = do
  putStrLn $ prettyPrintGameInfo game
  putStrLn $ prettyPrintBoxScore $ generateBoxScore game

main :: IO ()
main = do
  eventFile <- unpack . head <$> getArgs
  gamesFromFilePath eventFile >>= mapM_ printGameAndBoxScore
