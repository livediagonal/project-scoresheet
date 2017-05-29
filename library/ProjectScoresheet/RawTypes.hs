{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module ProjectScoresheet.RawTypes where

import ClassyPrelude
import Data.Csv
import Data.Finite
import qualified Data.Vector as V

data HomeOrAway = Away | Home deriving (Eq, Show)

instance FromField HomeOrAway where
  parseField "0" = pure Away
  parseField "1" = pure Home
  parseField val = fail $ "Unrecognized value: " ++ show val

newtype BattingPosition
  = BattingPosition
  { battingPosition :: Finite 10
  } deriving (Eq, Show)

instance FromField BattingPosition where
  parseField "0" = pure $ BattingPosition 0
  parseField "1" = pure $ BattingPosition 1
  parseField "2" = pure $ BattingPosition 2
  parseField "3" = pure $ BattingPosition 3
  parseField "4" = pure $ BattingPosition 4
  parseField "5" = pure $ BattingPosition 5
  parseField "6" = pure $ BattingPosition 6
  parseField "7" = pure $ BattingPosition 7
  parseField "8" = pure $ BattingPosition 8
  parseField "9" = pure $ BattingPosition 9
  parseField val = fail $ "Unrecognized value: " ++ show val

data EventFileLine
  = GameLine RawGameId
  | SchemaLine RawSchemaVersion
  | InfoLine RawInfo
  | CommentLine RawComment
  | StartLine RawStart
  | SubLine RawSub
  | PlayLine RawPlay
  | DataLine RawData
  deriving (Eq, Show, Generic)

data RawGameId
  = RawGameId
  { rawGameIdName :: !Text
  } deriving (Eq, Show, Generic)

data RawSchemaVersion
  = RawSchemaVersion
  { rawSchemaVersionId :: !Int
  } deriving (Eq, Show, Generic)

data RawComment
  = RawComment
   { rawComment :: !Text
  } deriving (Eq, Show, Generic)

data RawInfo
  = RawInfo
  { rawInfoKey :: !Text
  , rawInfoValue :: !Text
  } deriving (Eq, Show, Generic)

data RawStart
  = RawStart
  { rawStartPlayer :: !Text
  , rawStartPlayerName :: !Text
  , rawStartPlayerHome :: !HomeOrAway
  , rawStartBattingPosition :: !BattingPosition
  , rawStartFieldingPosition :: !Int
  } deriving (Eq, Show, Generic)

data RawSub
  = RawSub
  { rawSubPlayer :: !Text
  , rawSubPlayerName :: !Text
  , rawSubPlayerHome :: !HomeOrAway
  , rawSubBattingPosition :: !BattingPosition
  , rawSubFieldingPosition :: !Int
  } deriving (Eq, Show, Generic)

data RawPlay
  = RawPlay
  { rawPlayInning :: !Int
  , rawPlayOuts :: !Int
  , rawPlayPlayerId :: !Text
  , rawPlayCount :: !Int
  , rawPlayPitchSequence :: !Text
  , rawPlayResult :: !Text
  } deriving (Eq, Show, Generic)

data RawData
  = RawData
  { rawDataKey :: !Text
  , rawDataPlayerId :: !Text
  , rawDataValue :: !Int
  } deriving (Eq, Show, Generic)

instance FromRecord RawGameId
instance FromRecord RawSchemaVersion
instance FromRecord RawComment
instance FromRecord RawInfo
instance FromRecord RawStart
instance FromRecord RawSub
instance FromRecord RawPlay
instance FromRecord RawData

instance FromRecord EventFileLine where
  parseRecord v = do
   let args = V.tail v
   lineType <- v .! 0
   case (lineType :: Text)  of
     "id" -> GameLine <$> parseRecord args
     "version" -> SchemaLine <$> parseRecord args
     "play" -> PlayLine <$> parseRecord args
     "info" -> InfoLine <$> parseRecord args
     "start" -> StartLine <$> parseRecord args
     "sub" -> SubLine <$> parseRecord args
     "com" -> CommentLine <$> parseRecord args
     "data" -> DataLine <$> parseRecord args
     _ -> fail "Unrecognized key"
