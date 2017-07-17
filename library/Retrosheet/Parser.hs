{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Retrosheet.Parser where

import ClassyPrelude hiding (try)
import Control.Lens
import Data.Attoparsec.Text
import qualified Data.ByteString.Lazy as BL
import Data.Char (digitToInt)
import Data.Csv hiding (Parser)
import qualified Data.Vector as V

import Baseball.BaseballTypes
import Baseball.Game
import Baseball.Event hiding ( Event(..) )
import qualified Baseball.Event as P ( Event(..) )

import Retrosheet.Event

instance FromRecord IdEvent
instance FromRecord UnknownEvent
instance FromRecord SchemaEvent
instance FromRecord CommentEvent
instance FromRecord InfoEvent
instance FromRecord StartEvent
instance FromRecord SubEvent
instance FromRecord PlayEvent
instance FromRecord DataEvent

instance FromRecord Event where
  parseRecord v = do
   let args = V.tail v
   lineType <- v .! 0
   case (lineType :: Text)  of
     "id" -> IdEventType <$> parseRecord args
     "version" -> SchemaEventType <$> parseRecord args
     "play" -> PlayEventType . over _playEventResult saturatePlayMovements <$> parseRecord args
     "info" -> InfoEventType <$> parseRecord args
     "start" -> StartEventType <$> parseRecord args
     "sub" -> SubEventType <$> parseRecord args
     "com" -> CommentEventType <$> parseRecord args
     "data" -> DataEventType <$> parseRecord args
     _ -> UnknownEventType <$> parseRecord v

instance FromField Play where
  parseField info =
    case parseOnly parsePlay (decodeUtf8 info) of
      Left e -> fail e
      Right r -> pure r

parsePlay :: Parser Play
parsePlay = Play <$> "" <*> parsePlayAction `sepBy1` optional (char '+') <*> many parsePlayDescriptor <*> many parsePlayMovement

parsePlayAction :: Parser PlayAction
parsePlayAction =
  try parseBalk <|>
  try parseDefensiveIndifference <|>
  try parseStolenBase <|>
  try parseHit <|>
  try parseStrikeout <|>
  try parseRoutinePlay <|>
  try parseFieldersChoice <|>
  try parseCaughtStealing <|>
  try parsePickoff <|>
  try parseWildPitch <|>
  try parsePassedBall <|>
  try parseWalk <|>
  try parseNoPlay <|>
  try parseHitByPitch <|>
  try parseErrorFoulFly <|>
  try parseError

parseBalk :: Parser PlayAction
parseBalk = string "BK" *> pure Balk

parseStolenBase :: Parser PlayAction
parseStolenBase = string "SB" *> (StolenBase <$> parseNumericBase)

parseCaughtStealing :: Parser PlayAction
parseCaughtStealing = string "CS" *> (CaughtStealing <$> parseNumericBase <*> optional (parseParenthetical parseFieldingPositions))

parsePickoff :: Parser PlayAction
parsePickoff = do
  pickoff <- try (string "POCS" *> pure (Pickoff True)) <|> (string "PO" *> pure (Pickoff False))
  pickoff <$> parseNumericBase <*> map isJust (optional (parseParenthetical parseError)) <*> optional (parseParenthetical parseFieldingPositions)

parseParenthetical :: Parser a -> Parser a
parseParenthetical delegate = char '(' *> delegate <* char ')'

parseHit :: Parser PlayAction
parseHit = Hit <$> parseBase <*> optional (some parseFieldingPosition)

parseRoutinePlay :: Parser PlayAction
parseRoutinePlay = RoutinePlay <$> parseFieldingPositions <*> optional parseOutRunnerBase

parseFieldersChoice :: Parser PlayAction
parseFieldersChoice = string "FC" *> map FieldersChoice parseFieldingPositions

parseOutRunnerBase :: Parser Base
parseOutRunnerBase = parseParenthetical parseNumericBase

parseFieldingPositions :: Parser [FieldingPosition]
parseFieldingPositions = some parseFieldingPosition

parseFieldingPosition :: Parser FieldingPosition
parseFieldingPosition = fieldPositionFromId . digitToInt <$> digit

parseBase :: Parser Base
parseBase =
  try (char 'S' *> pure FirstBase) <|>
  try (string "DGR" *> pure SecondBase) <|>
  try (char 'D' *> pure SecondBase) <|>
  try (char 'T' *> pure ThirdBase) <|>
  try (string "HR" *> pure HomePlate)

parseWildPitch :: Parser PlayAction
parseWildPitch =
  try (string "WP" *> pure WildPitch)

parsePassedBall :: Parser PlayAction
parsePassedBall =
  try (string "PB" *> pure PassedBall)

parseDefensiveIndifference :: Parser PlayAction
parseDefensiveIndifference =
  try (string "DI" *> pure DefensiveIndifference)

parseWalk :: Parser PlayAction
parseWalk =
  try (char 'W' *> pure (Walk False)) <|>
  try (string "IW" *> pure (Walk True)) <|>
  try (char 'I' *> pure (Walk True))

parseStrikeout :: Parser PlayAction
parseStrikeout =
  try (string "K23" *> pure (Strikeout Nothing)) <|>
  try (char 'K' *> pure (Strikeout Nothing))

parseNoPlay :: Parser PlayAction
parseNoPlay =
  const NoPlay <$> (try (void $ string "NP") <|> void (string "OA"))

parseHitByPitch :: Parser PlayAction
parseHitByPitch = string "HP" *> pure HitByPitch

parseError :: Parser PlayAction
parseError = do
  mThrower <- optional parseFieldingPosition
  char 'E' *> map (flip Error mThrower) parseFieldingPosition

parseErrorFoulFly :: Parser PlayAction
parseErrorFoulFly =
  string "FLE" *> map ErrorFoulFly parseFieldingPosition

parsePlayDescriptor :: Parser PlayDescriptor
parsePlayDescriptor = do
  void $ char '/'
  try (string "FO" *> pure ForceOut) <|>
    try (string "SF" *> pure SacrificeFly) <|>
    try (string "SH" *> pure SacrificeBunt) <|>
    OtherDescriptor . pack <$> many (satisfy (not . \c -> c == '/' || c == '.'))

parseNumericBase :: Parser Base
parseNumericBase =
  try (char 'B' *> pure HomePlate) <|>
  try (char '1' *> pure FirstBase) <|>
  try (char '2' *> pure SecondBase) <|>
  try (char '3' *> pure ThirdBase) <|>
  char 'H' *> pure HomePlate

parsePlayMovementFieldingSequence :: Parser PlayMovementDescriptor
parsePlayMovementFieldingSequence =
  try (char 'E' *> (PlayMovementFieldingSequence True <$> parseFieldingPositions) <* optional (string "/TH")) <|>
  (PlayMovementFieldingSequence False <$> parseFieldingPositions) <* optional (string "/TH")

parseMovementAnnotation :: Parser PlayMovementDescriptor
parseMovementAnnotation =
  try (parseParenthetical (string "NR") *> pure PlayMovementNoRBI) <|>
  try (parseParenthetical (string "UR") *> pure PlayMovementUnearned) <|>
  try (parseParenthetical parsePlayMovementFieldingSequence)

parsePlayMovement :: Parser PlayMovement
parsePlayMovement = do
  void $ try (char '.') <|> char ';'
  startBase <- parseNumericBase
  isSuccess <- try (char 'X' *> pure False) <|> char '-' *> pure True
  endBase <- parseNumericBase
  PlayMovement startBase endBase isSuccess <$> many parseMovementAnnotation

-- Sandbox --


gamesFromFilePath :: String -> IO [Game]
gamesFromFilePath file = do
  events <- retrosheetEventsFromFile file
  pure $ reverse (foldl' (flip generateGames) [] events)

retrosheetEventsFromFile :: String -> IO [Event]
retrosheetEventsFromFile file = do
  csvEvents <- BL.readFile file
  case (decode NoHeader csvEvents :: Either String (Vector Event)) of
    Left err -> fail err
    Right v -> pure $ toList v

generateGames :: Event -> [Game] -> [Game]
generateGames (IdEventType _) games = initialGame : games
generateGames event (g:rest) = updateGame event g : rest
generateGames _ games = games

updateGame :: Event -> Game -> Game
updateGame (InfoEventType event) = processInfoEvent event
updateGame (StartEventType event) = processStartEvent event
updateGame (SubEventType event) = processSubEvent event
updateGame (PlayEventType event) = processPlayEvent event
updateGame _ = id

toSimpleEvent :: Event -> Maybe P.Event
toSimpleEvent (StartEventType StartEvent{..}) = Just $ P.SubstitutionEvent (Substitution startEventPlayer startEventPlayerHome startEventBattingPosition startEventFieldingPosition)
toSimpleEvent (SubEventType SubEvent{..}) = Just $ P.SubstitutionEvent (Substitution subEventPlayer subEventPlayerHome subEventBattingPosition subEventFieldingPosition)
toSimpleEvent (PlayEventType PlayEvent{..}) = Just $ P.PlayEvent $ playEventResult {playPlayer = playEventPlayerId}
toSimpleEvent _ = Nothing

processInfoEvent :: InfoEvent -> Game -> Game
processInfoEvent InfoEvent{..} = do
  let info = Just infoEventValue
  case infoEventKey of
    "visteam" -> _gameAwayTeam .~ info
    "hometeam" -> _gameHomeTeam .~ info
    "date" -> _gameDate .~ info
    "starttime" -> _gameStartTime .~ info
    _ -> id

processStartEvent :: StartEvent -> Game -> Game
processStartEvent StartEvent{..} =
  let
    gameEvent = P.SubstitutionEvent (Substitution startEventPlayer startEventPlayerHome startEventBattingPosition startEventFieldingPosition)
  in
    addEventToGame gameEvent

processSubEvent :: SubEvent -> Game -> Game
processSubEvent SubEvent{..} =
  let
    gameEvent = P.SubstitutionEvent (Substitution subEventPlayer subEventPlayerHome subEventBattingPosition subEventFieldingPosition)
  in
    addEventToGame gameEvent

processPlayEvent :: PlayEvent -> Game -> Game
processPlayEvent PlayEvent{..} =
  let
    updatedPlay = playEventResult {playPlayer = playEventPlayerId}
    gameEvent = P.PlayEvent updatedPlay
  in
    addEventToGame gameEvent
