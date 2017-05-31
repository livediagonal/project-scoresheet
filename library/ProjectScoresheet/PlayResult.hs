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
import Data.Attoparsec.Text
import Data.Csv hiding (Parser)

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

data PlayAction
  = Strikeout
  | Outs
  | Hit Int Int
  | Walk Bool
  | NoPlay
  | Other Text
  deriving (Eq, Show)

parsePlayResult :: Parser PlayResult
parsePlayResult = do
  playAction <- parsePlayAction
  playDescriptors <- many parsePlayDescriptor
  playMovements <- many parsePlayMovement
  return $ PlayResult playAction playDescriptors playMovements

parsePlayAction :: Parser PlayAction
parsePlayAction = try parseStrikeout <|> try parseNoPlay <|> try parseOther

parsePlayActionToken :: Text -> PlayAction -> Parser PlayAction
parsePlayActionToken token result = do
  void $ string token
  void $ many (satisfy (not . \c -> c == '/' || c == '.'))
  pure result

parseStrikeout :: Parser PlayAction
parseStrikeout = parsePlayActionToken "K" Strikeout

parseNoPlay :: Parser PlayAction
parseNoPlay = parsePlayActionToken "NP" NoPlay

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
