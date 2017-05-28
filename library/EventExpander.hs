{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module EventExpander where

import ClassyPrelude
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

data Event
  = Event
  { eventNumber :: !Int
  , numOuts :: !Int
  , inning :: !Int
  , isLeadOff :: !Bool
  } deriving (Eq, Show, Generic)

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
  , rawInfoVal :: !Text
  } deriving (Eq, Show, Generic)

data RawStart
  = RawStart
  { rawStartPlayer :: !Text
  , rawStartPlayerName :: !Text
  , rawStartPlayerHome :: !Int
  , rawStartBattingOrderPosition :: !Int
  , rawStartFieldingPosition :: !Int
  } deriving (Eq, Show, Generic)

data RawSub
  = RawSub
  { rawSubPlayer :: !Text
  , rawSubPlayerName :: !Text
  , rawSubPlayerHome :: !Int
  , rawSubBattingOrderPosition :: !Int
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

processEvent :: Event -> EventFileLine -> Event
processEvent prevEvent (PlayLine RawPlay{..}) =
  let
    evNumber = eventNumber prevEvent + 1
    isNewInning = rawPlayInning /= inning prevEvent
    isOutsReset = rawPlayOuts /= numOuts prevEvent && rawPlayOuts == 0
  in
    prevEvent
     { eventNumber = evNumber
     , numOuts = rawPlayOuts
     , inning = rawPlayInning
     , isLeadOff = isOutsReset || isNewInning
     }
processEvent prevEvent _ = prevEvent

main :: IO ()
main = do
  csvEvents <- BL.readFile "testgame.txt"
  case (decode NoHeader csvEvents :: Either String (Vector EventFileLine)) of
    Left err -> print err
    Right v -> V.mapM_ print $ V.tail $ V.scanl processEvent (Event 0 0 0 False) $ V.filter (\e -> case e of PlayLine RawPlay{..} -> True; _ -> False) v
