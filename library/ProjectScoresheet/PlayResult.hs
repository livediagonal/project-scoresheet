{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module ProjectScoresheet.PlayResult where

import ClassyPrelude hiding (try)
import Data.Char (digitToInt, isDigit)
import Data.Attoparsec.Text
import Data.Csv hiding (Parser)
import ProjectScoresheet.BaseballTypes

data PlayResult
  = PlayResult
  { playResultAction :: !PlayAction
  , playResultDescriptors :: ![Text]
  , playResultMovements :: ![PlayMovement]
  } deriving (Eq, Show, Generic)

instance FromField PlayResult where
  parseField info =
    case parseOnly parsePlayResult (decodeUtf8 info) of
      Left e -> fail e
      Right r -> pure r

data Base
  = FirstBase
  | SecondBase
  | ThirdBase
  | HomePlate
  deriving (Eq, Show, Enum)

data Out
  = RoutinePlay [FieldingPosition]
  | FieldersChoice [FieldingPosition]
  | Strikeout (Maybe Text)
  deriving (Eq, Show)

data PlayAction
  = Outs [Out]
  | Hit Base (Maybe FieldingPosition)
  | Walk Bool
  | NoPlay (Maybe Text)
  | Other Text
  | HitByPitch
  | Error FieldingPosition
  deriving (Eq, Show)

parsePlayResult :: Parser PlayResult
parsePlayResult = do
  playAction <- parsePlayAction
  playDescriptors <- many parsePlayDescriptor
  playMovements <- many parsePlayMovement
  pure $ PlayResult playAction playDescriptors playMovements

parsePlayAction :: Parser PlayAction
parsePlayAction =
  try parseHit <|>
  try parseOuts <|>
  try parseWalk <|>
  try parseNoPlay <|>
  try parseHitByPitch <|>
  try parseError <|>
  try parseOther

parseHit :: Parser PlayAction
parseHit = Hit <$> parseBase <*> optional parseFieldingPosition

parseOuts :: Parser PlayAction
parseOuts = Outs <$> some parseOut

parseOut :: Parser Out
parseOut =
  try parseStrikeout <|>
  try parseFieldersChoice <|>
  try (RoutinePlay <$> parseFieldingPositions)

parseFieldersChoice :: Parser Out
parseFieldersChoice = string "FC" *> map FieldersChoice parseFieldingPositions

parseFieldingPositions :: Parser [FieldingPosition]
parseFieldingPositions = some parseFieldingPosition <* skipMany (satisfy (\c -> c == '(' || c == ')' || isDigit c))

parseFieldingPosition :: Parser FieldingPosition
parseFieldingPosition = fieldPositionFromId . digitToInt <$> digit

parseBase :: Parser Base
parseBase =
  try (char 'S' *> pure FirstBase) <|>
  try (char 'D' *> pure SecondBase) <|>
  try (char 'T' *> pure ThirdBase) <|>
  try (string "HR" *> pure HomePlate)

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
parseStrikeout = parsePlayActionTokenWithQualifier "K" Strikeout

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

parsePlayDescriptor :: Parser Text
parsePlayDescriptor = do
  void $ char '/'
  pack <$> many (satisfy (not . \c -> c == '/' || c == '.'))

parseNumericBase :: Parser Base
parseNumericBase =
  try (char 'B' *> pure HomePlate) <|>
  try (char '1' *> pure FirstBase) <|>
  try (char '2' *> pure SecondBase) <|>
  try (char '3' *> pure ThirdBase) <|>
  char 'H' *> pure HomePlate

parsePlayMovement :: Parser PlayMovement
parsePlayMovement = do
  void $ try (char '.') <|> char ';'
  startBase <- parseNumericBase
  isSuccess <- try (char 'X' *> pure False) <|> anyChar *> pure True
  endBase <- parseNumericBase
  pure $ PlayMovement startBase endBase isSuccess

data PlayMovement = PlayMovement Base Base Bool deriving (Eq, Show)

isBatterEvent :: PlayResult -> Bool
isBatterEvent PlayResult{..} = False

isHit :: PlayResult -> Bool
isHit PlayResult{..} =
  case playResultAction of
    Hit _ _ -> True
    _ -> False

isRBI :: PlayMovement -> Bool
isRBI (PlayMovement _ HomePlate True) = True
isRBI _ = False

numRBI :: PlayResult -> Int
numRBI PlayResult{..} = length $ filter isRBI playResultMovements

isAtBat :: PlayResult -> Bool
isAtBat PlayResult{..} = False

isBattedBall :: PlayResult -> Bool
isBattedBall PlayResult{..} = False

isStrikeout :: PlayResult -> Bool
isStrikeout PlayResult{..} = False

isDoublePlay :: PlayResult -> Bool
isDoublePlay PlayResult{..} = False

isTriplePlay :: PlayResult -> Bool
isTriplePlay PlayResult{..} = False

isWildPitch :: PlayResult -> Bool
isWildPitch PlayResult{..} = False

isPassedBall :: PlayResult -> Bool
isPassedBall PlayResult{..} = False

--   { playResultIsBatterEvent :: !Bool
--   , playResultIsAtBat :: !Bool
--   , playResultIsHit :: !Bool
--   , playResultIsBattedBall :: !Bool
--   , playResultIsBunt :: !Bool
--   , playResultHitLocation :: !(Maybe Text)
--   , playResultIsStrikeout :: !Bool
--   , playResultIsDoublePlay :: !Bool
--   , playResultIsTriplePlay :: !Bool
--   , playResultIsWildPitch :: !Bool
--   , playResultIsPassedBall :: !Bool
--   , playResultNumErrors :: !Int
--   , playResultFieldedById :: !(Maybe Text)
--   , playResultDidRunnerOnFirstSteal :: !Bool
--   , playResultDidRunnerOnSecondSteal :: !Bool
--   , playResultDidRunnerOnThirdSteal :: !Bool
--   , playResultWasRunnerOnFirstCaughtStealing :: !Bool
--   , playResultWasRunnerOnSecondCaughtStealing :: !Bool
--   , playResultWasRunnerOnThirdCaughtStealing :: !Bool
--   , playResultAction :: !Text
--   , playResultDescriptors :: ![Text]
--   , playResultMovements :: ![Text]

