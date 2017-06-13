{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module ProjectScoresheet.PlayResult where

import ClassyPrelude hiding (try)
import Control.Lens
import Data.Char (digitToInt, isDigit)
import Data.Attoparsec.Text
import Data.Csv hiding (Parser)
import ProjectScoresheet.BaseballTypes

data Base
  = FirstBase
  | SecondBase
  | ThirdBase
  | HomePlate
  deriving (Eq, Show, Enum)

data Out
  = RoutinePlay [FieldingPosition] (Maybe Base)
  | FieldersChoice [FieldingPosition]
  | Strikeout (Maybe Text)
  deriving (Eq, Show)

data PlayAction
  = Outs [Out]
  | Hit Base (Maybe FieldingPosition)
  | StolenBase Base
  | WildPitch
  | Walk Bool
  | NoPlay (Maybe Text)
  | Other Text
  | HitByPitch
  | Error FieldingPosition
  deriving (Eq, Show)

data PlayDescriptor
  = ForceOut
  | SacrificeFly
  | SacrificeBunt
  | OtherDescriptor Text
  deriving (Eq, Show)

data PlayMovement = PlayMovement Base Base Bool deriving (Eq, Show)

data PlayResult
  = PlayResult
  { playResultAction :: !PlayAction
  , playResultDescriptors :: ![PlayDescriptor]
  , playResultMovements :: ![PlayMovement]
  } deriving (Eq, Show, Generic)

makeClassy_ ''PlayResult

instance FromField PlayResult where
  parseField info =
    case parseOnly parsePlayResult (decodeUtf8 info) of
      Left e -> fail e
      Right r -> pure r

parsePlayResult :: Parser PlayResult
parsePlayResult = do
  playAction <- parsePlayAction
  playDescriptors <- many parsePlayDescriptor
  playMovements <- many parsePlayMovement
  pure $ PlayResult playAction playDescriptors playMovements

parsePlayAction :: Parser PlayAction
parsePlayAction =
  try parseStolenBase <|>
  try parseHit <|>
  try parseOuts <|>
  try parseWildPitch <|>
  try parseWalk <|>
  try parseNoPlay <|>
  try parseHitByPitch <|>
  try parseError <|>
  try parseOther

parseStolenBase :: Parser PlayAction
parseStolenBase = string "SB" *> (StolenBase <$> parseNumericBase)

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

parsePlayMovement :: Parser PlayMovement
parsePlayMovement = do
  void $ try (char '.') <|> char ';'
  startBase <- parseNumericBase
  isSuccess <- try (char 'X' *> pure False) <|> char '-' *> pure True
  endBase <- parseNumericBase
  void $ many (satisfy (\c -> c == '(' || c == ')' || isDigit c))
  pure $ PlayMovement startBase endBase isSuccess

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

fromBool :: Bool -> Int
fromBool False = 0
fromBool True = 1

numRBI :: PlayResult -> Int
numRBI PlayResult{..} = length (filter isRBI playResultMovements)

isHomeRun :: PlayResult -> Bool
isHomeRun PlayResult{..} =
  case playResultAction of
    Hit HomePlate _ -> True
    _ -> False

isAtBat :: PlayResult -> Bool
isAtBat pr@PlayResult{..} =
  case playResultAction of
    Walk _ -> False
    HitByPitch -> False
    WildPitch -> False
    NoPlay _ -> False
    Other _ -> False
    StolenBase _ -> False
    Outs _ -> not $ isSacrifice pr
    _ -> True

isWalk :: PlayResult -> Bool
isWalk PlayResult{..} =
  case playResultAction of
    Walk _ -> True
    _ -> False

isStrikeout :: PlayResult -> Bool
isStrikeout PlayResult{..} =
  case playResultAction of
    Outs outs -> any isStrikeoutOut outs
    _ -> False

isStrikeoutOut :: Out -> Bool
isStrikeoutOut out =
  case out of
    Strikeout _ -> True
    _ -> False

isForceOut :: PlayResult -> Bool
isForceOut (PlayResult _ descriptors _) = any isForceOutDescriptor descriptors

isForceOutDescriptor :: PlayDescriptor -> Bool
isForceOutDescriptor ForceOut = True
isForceOutDescriptor _ = False

isSacrifice :: PlayResult -> Bool
isSacrifice (PlayResult _ descriptors _) = any isSacrificeDescriptor descriptors

isSacrificeDescriptor :: PlayDescriptor -> Bool
isSacrificeDescriptor SacrificeFly = True
isSacrificeDescriptor SacrificeBunt = True
isSacrificeDescriptor _ = False

isBattedBall :: PlayResult -> Bool
isBattedBall PlayResult{..} = False

isDoublePlay :: PlayResult -> Bool
isDoublePlay PlayResult{..} = False

isTriplePlay :: PlayResult -> Bool
isTriplePlay PlayResult{..} = False

isWildPitch :: PlayResult -> Bool
isWildPitch PlayResult{..} = False

isPassedBall :: PlayResult -> Bool
isPassedBall PlayResult{..} = False

addPlayMovement :: PlayMovement -> [PlayMovement] -> [PlayMovement]
addPlayMovement pm@(PlayMovement startBase _ _) pms =
  case any (\(PlayMovement existingStartBase _ _) -> startBase == existingStartBase) pms of
    True -> pms
    False -> pms ++ [pm]

saturatePlayMovements :: PlayResult -> PlayResult
saturatePlayMovements pr@PlayResult{..} =
  let
    pr' = case playResultAction of
      Walk _ -> over _playResultMovements (addPlayMovement (PlayMovement HomePlate FirstBase True)) pr
      HitByPitch -> over _playResultMovements (addPlayMovement (PlayMovement HomePlate FirstBase True)) pr
      Hit base _ -> over _playResultMovements (addPlayMovement (PlayMovement HomePlate base True)) pr
      Outs outs -> foldr saturateMovementsOnOut pr outs
      _ -> pr
  in
    case isForceOut pr' of
      True -> over _playResultMovements (addPlayMovement (PlayMovement HomePlate FirstBase True)) pr'
      False -> pr'

saturateMovementsOnOut :: Out -> PlayResult -> PlayResult
saturateMovementsOnOut (FieldersChoice _) pr = over _playResultMovements (addPlayMovement (PlayMovement HomePlate FirstBase True)) pr
saturateMovementsOnOut (RoutinePlay _ (Just startingBase)) pr = over _playResultMovements (addPlayMovement (PlayMovement startingBase HomePlate False)) pr
saturateMovementsOnOut _ pr = pr
