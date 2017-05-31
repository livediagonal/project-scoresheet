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
  , playResultMovements :: ![Text]
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
  = RoutinePlay [FieldPosition]
  | FieldersChoice [FieldPosition]
  | Strikeout (Maybe Text)
  deriving (Eq, Show)

data PlayAction
  = Outs [Out]
  | Hit Base (Maybe FieldPosition)
  | Walk Bool
  | NoPlay (Maybe Text)
  | Other Text
  deriving (Eq, Show)

parsePlayResult :: Parser PlayResult
parsePlayResult = do
  playAction <- parsePlayAction
  playDescriptors <- many parsePlayDescriptor
  playMovements <- many parsePlayMovement
  return $ PlayResult playAction playDescriptors playMovements

parsePlayAction :: Parser PlayAction
parsePlayAction =
  try parseHit <|>
  try parseOuts <|>
  try parseWalk <|>
  try parseNoPlay <|>
  try parseOther

parseHit :: Parser PlayAction
parseHit = do
  base <- parseBase
  fieldedBy <- try (Just <$> parseFieldPosition) <|> pure Nothing
  pure $ Hit base fieldedBy

parseFieldPosition :: Parser FieldPosition
parseFieldPosition = fieldPositionFromId . digitToInt <$> digit

parseOuts :: Parser PlayAction
parseOuts = Outs <$> some parseOut

parseOut :: Parser Out
parseOut =
  try parseStrikeout <|>
  try parseFieldersChoice <|>
  try (RoutinePlay <$> parseFieldPositions)

parseFieldersChoice :: Parser Out
parseFieldersChoice = do
  void $ string "FC"
  FieldersChoice <$> parseFieldPositions

parseFieldPositions :: Parser [FieldPosition]
parseFieldPositions = do
  positions <- some parseFieldPosition
  void $ many (satisfy (\c -> c == '(' || c == ')' || isDigit c))
  return positions

parseBase :: Parser Base
parseBase =
  try (char 'S' *> pure FirstBase) <|>
  try (char 'D' *> pure SecondBase) <|>
  try (char 'T' *> pure ThirdBase) <|>
  try (string "HR" *> pure HomePlate)

parseWalk :: Parser PlayAction
parseWalk =
  try (char 'W' *> pure (Walk False)) <|>
  try (string "IW" *> pure (Walk True))

parsePlayActionTokenWithQualifier :: Text -> (Maybe Text -> a) -> Parser a
parsePlayActionTokenWithQualifier token result = do
  void $ string token
  qualifier <- try parseQualifier <|> pure Nothing
  pure $ result qualifier

parseQualifier :: Parser (Maybe Text)
parseQualifier = do
  void $ char '+'
  Just . pack <$> many (satisfy (not . \c -> c == '/' || c == '.'))

parseStrikeout :: Parser Out
parseStrikeout = parsePlayActionTokenWithQualifier "K" Strikeout

parseNoPlay :: Parser PlayAction
parseNoPlay = parsePlayActionTokenWithQualifier "NP" NoPlay

parseOther :: Parser PlayAction
parseOther = Other . pack <$> many (satisfy (not . \c -> c == '/' || c == '.'))

parsePlayDescriptor :: Parser Text
parsePlayDescriptor = do
  void $ char '/'
  pack <$> many (satisfy (not . \c -> c == '/' || c == '.'))

parsePlayMovement :: Parser Text
parsePlayMovement = do
  void (char '.' <|> char ';')
  pack <$> many (satisfy (not . \c -> c == ';'))


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

isBatterEvent :: PlayResult -> Bool
isBatterEvent PlayResult{..} = False

isHit :: PlayResult -> Bool
isHit PlayResult{..} = False

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
