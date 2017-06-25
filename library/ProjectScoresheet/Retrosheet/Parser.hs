{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ProjectScoresheet.Retrosheet.Parser where

import ClassyPrelude hiding (try)
import Control.Lens
import Data.Attoparsec.Text
import qualified Data.ByteString.Lazy as BL
import Data.Char (digitToInt, isDigit)
import Data.Csv hiding (Parser)
import qualified Data.Vector as V

import ProjectScoresheet.BaseballTypes
import ProjectScoresheet.Play
import ProjectScoresheet.Retrosheet.Events

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
parsePlay = Play <$> parsePlayAction <*> many parsePlayDescriptor <*> many parsePlayMovement

parsePlayAction :: Parser PlayAction
parsePlayAction =
  try parseStolenBase <|>
  try parseHit <|>
  try parseOuts <|>
  try parseCaughtStealing <|>
  try parseWildPitch <|>
  try parseWalk <|>
  try parseNoPlay <|>
  try parseHitByPitch <|>
  try parseError <|>
  try parseOther

parseStolenBase :: Parser PlayAction
parseStolenBase = string "SB" *> (StolenBase <$> parseNumericBase)

parseCaughtStealing :: Parser PlayAction
parseCaughtStealing = string "CS" *> (CaughtStealing <$> parseNumericBase <*> optional (parseParenthetical parseFieldingPositions))

parseParenthetical :: Parser a -> Parser a
parseParenthetical delegate = char '(' *> delegate <* char ')'

parseHit :: Parser PlayAction
parseHit = Hit <$> parseBase <*> optional parseFieldingPosition

parseOuts :: Parser PlayAction
parseOuts = Outs <$> some parseOut

parseOut :: Parser Out
parseOut =
  try parseStrikeout <|>
  try parseFieldersChoice <|>
  try (RoutinePlay <$> parseFieldingPositions <*> optional parseOutRunnerBase)

parseFieldersChoice :: Parser Out
parseFieldersChoice = string "FC" *> map FieldersChoice parseFieldingPositions

parseOutRunnerBase :: Parser Base
parseOutRunnerBase = do
  void $ char '('
  base <- parseNumericBase
  void $ char ')'
  return base

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

parseWalk :: Parser PlayAction
parseWalk =
  try (char 'W' *> pure (Walk False)) <|>
  try (string "IW" *> pure (Walk True)) <|>
  try (char 'I' *> pure (Walk True))

parsePlayActionTokenWithQualifier :: Text -> (Maybe Text -> a) -> Parser a
parsePlayActionTokenWithQualifier token result = string token *> (result <$> optional parseQualifier)

parseQualifier :: Parser Text
parseQualifier = char '+' *> (pack <$> many (satisfy (not . \c -> c == '/' || c == '.')))

parseStrikeout :: Parser Out
parseStrikeout =
  try (string "K23" *> pure (Strikeout Nothing)) <|>
  parsePlayActionTokenWithQualifier "K" Strikeout

parseNoPlay :: Parser PlayAction
parseNoPlay = parsePlayActionTokenWithQualifier "NP" NoPlay

parseHitByPitch :: Parser PlayAction
parseHitByPitch = string "HP" *> pure HitByPitch

parseError :: Parser PlayAction
parseError = do
  void $ char 'E'
  Error <$> try parseFieldingPosition

parseOther :: Parser PlayAction
parseOther = Other . pack <$> many (satisfy (not . \c -> c == '/' || c == '.'))

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

parseMovementAnnotation :: Parser ()
parseMovementAnnotation = do
  void $ char '('
  void $ try (string "NR") <|> pack <$> many (satisfy isDigit)
  void $ char ')'

parsePlayMovement :: Parser PlayMovement
parsePlayMovement = do
  void $ try (char '.') <|> char ';'
  startBase <- parseNumericBase
  isSuccess <- try (char 'X' *> pure False) <|> char '-' *> pure True
  endBase <- parseNumericBase
  void $ optional parseMovementAnnotation
  pure $ PlayMovement startBase endBase isSuccess

retrosheetEventsFromFile :: String -> IO [Event]
retrosheetEventsFromFile file = do
  csvEvents <- BL.readFile file
  case (decode NoHeader csvEvents :: Either String (Vector Event)) of
    Left err -> fail err
    Right v -> pure $ toList v
