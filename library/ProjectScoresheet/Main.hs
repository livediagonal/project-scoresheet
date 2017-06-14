{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ProjectScoresheet.Main where

import ClassyPrelude
import ProjectScoresheet.BoxScore
import ProjectScoresheet.Print

main :: IO ()
main = prettyPrintBoxScore <$> boxScoreFromFile "testgame.txt" >>= putStrLn
