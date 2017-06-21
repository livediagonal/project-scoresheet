{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ProjectScoresheet.Main where

import ClassyPrelude hiding (head)
import Data.List (head)
import ProjectScoresheet.GameState
import ProjectScoresheet.Print
import ProjectScoresheet.BoxScore

main :: IO ()
main = do
  eventFile <- unpack . head <$> getArgs
  gamesFromFilePath eventFile >>= mapM_ (putStrLn . prettyPrintBoxScore . generateBoxScore)
