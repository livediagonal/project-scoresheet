{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import ClassyPrelude
import Data.Csv
import Data.Text.Lazy.IO hiding (putStr, putStrLn)
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.IO hiding (getLine, putStr, putStrLn)

import Baseball.BoxScore
import Baseball.Game.GameEvent
import Retrosheet.Event
import Retrosheet.Parser

main :: IO ()
main =
  hSetBuffering stdout NoBuffering >> loop Nothing initialBoxScore
  where
    loop mGameEvent boxScore = do
      putStr "> "
      rawEvents <- encodeUtf8 <$> getLine
      parsedEvents <- case (decode NoHeader rawEvents :: Either String (Vector Event)) of
        Left err -> fail err
        Right v -> pure $ mapMaybe toSimpleEvent $ V.toList v
      case parsedEvents of
        [] -> loop mGameEvent boxScore
        allEvents@(firstEvent:events) -> do
          let updateState e (ge, bs) =
                let next = nextGameEvent e ge
                in (next, addEventToBoxScore next bs)
              (ge', boxScore') = case mGameEvent of
                Nothing -> foldr updateState (initialGameEvent firstEvent, boxScore) events
                Just ge -> foldr updateState (ge, boxScore) allEvents
          putStrLn $ prettyPrintGameEvent ge'
          putStrLn $ prettyPrintBoxScore boxScore'
          loop (Just ge') boxScore'
