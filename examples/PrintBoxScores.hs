{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import ClassyPrelude hiding (head)
import Data.List (head)

import Baseball.BoxScore
import Baseball.Game

main :: IO ()
main = do
  eventFile <- unpack . head <$> getArgs
  gamesFromFilePath eventFile >>= mapM_ (putStrLn . prettyPrintBoxScore . generateBoxScore)
