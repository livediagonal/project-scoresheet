{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module ProjectScoresheet.PlayResult where

import ClassyPrelude hiding (try)
import Data.Attoparsec.Text
import Data.Csv hiding (Parser)

data PlayResult
  = PlayResult
  { playResultIsHit :: !Bool
  , playResultIsStrikeout :: !Bool
  , playResultAction :: !Text
  , playResultDescriptors :: ![Text]
  , playResultMovements :: ![Text]
  } deriving (Eq, Show, Generic)

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
  return $ PlayResult False False playAction playDescriptors playMovements

parsePlayAction :: Parser Text
parsePlayAction = pack <$> many1 (satisfy (not . \c -> c == '/' || c == '.'))

parsePlayDescriptor :: Parser Text
parsePlayDescriptor = do
  void $ char '/'
  pack <$> many (satisfy (not . \c -> c == '/' || c == '.'))

parsePlayMovement :: Parser Text
parsePlayMovement = do
  void (char '.' <|> char ';')
  pack <$> many (satisfy (not . \c -> c == ';'))
