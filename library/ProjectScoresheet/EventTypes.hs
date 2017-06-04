{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module ProjectScoresheet.EventTypes where

import ClassyPrelude
import Data.Csv
import ProjectScoresheet.BaseballTypes
import ProjectScoresheet.PlayResult
import qualified Data.Vector as V

data Event
  = IdEventType IdEvent
  | SchemaEventType SchemaEvent
  | InfoEventType InfoEvent
  | CommentEventType CommentEvent
  | StartEventType StartEvent
  | SubEventType SubEvent
  | PlayEventType PlayEvent
  | DataEventType DataEvent
  | EmptyEvent
  deriving (Eq, Show, Generic)

data IdEvent
  = IdEvent
  { idEventName :: !Text
  } deriving (Eq, Show, Generic)

data SchemaEvent
  = SchemaEvent
  { schemaEventVersionId :: !Int
  } deriving (Eq, Show, Generic)

data CommentEvent
  = CommentEvent
  { commentEventBody :: !Text
  } deriving (Eq, Show, Generic)

data InfoEvent
  = InfoEvent
  { infoEventKey :: !Text
  , infoEventValue :: !Text
  } deriving (Eq, Show, Generic)

data StartEvent
  = StartEvent
  { startEventPlayer :: !Text
  , startEventPlayerName :: !Text
  , startEventPlayerHome :: !HomeOrAway
  , startEventBattingPosition :: !BattingOrderPosition
  , startEventFieldingPosition :: !FieldingPositionId
  } deriving (Eq, Show, Generic)

data SubEvent
  = SubEvent
  { subEventPlayer :: !Text
  , subEventPlayerName :: !Text
  , subEventPlayerHome :: !HomeOrAway
  , subEventBattingPosition :: !BattingOrderPosition
  , subEventFieldingPosition :: !FieldingPositionId
  } deriving (Eq, Show, Generic)

data PlayEvent
  = PlayEvent
  { playEventInning :: !Int
  , playEventOuts :: !Int
  , playEventPlayerId :: !Text
  , playEventCount :: !Int
  , playEventPitchSequence :: !Text
  , playEventResult :: !PlayResult
  } deriving (Eq, Show, Generic)

data DataEvent
  = DataEvent
  { dataEventKey :: !Text
  , dataEventPlayerId :: !Text
  , dataEventValue :: !Int
  } deriving (Eq, Show, Generic)

instance FromRecord IdEvent
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
     "play" -> PlayEventType <$> parseRecord args
     "info" -> InfoEventType <$> parseRecord args
     "start" -> StartEventType <$> parseRecord args
     "sub" -> SubEventType <$> parseRecord args
     "com" -> CommentEventType <$> parseRecord args
     "data" -> DataEventType <$> parseRecord args
     _ -> fail "Unrecognized key"
