{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Retrosheet.Event where

import ClassyPrelude
import Control.Lens

import Baseball.BaseballTypes
import Baseball.Event

data Event
  = IdEventType IdEvent
  | SchemaEventType SchemaEvent
  | InfoEventType InfoEvent
  | CommentEventType CommentEvent
  | StartEventType StartEvent
  | SubEventType SubEvent
  | PlayEventType PlayEvent
  | DataEventType DataEvent
  | UnknownEventType UnknownEvent
  | EmptyEvent
  deriving (Eq, Show, Generic)

data IdEvent
  = IdEvent
  { idEventName :: !Text
  } deriving (Eq, Show, Generic)

data UnknownEvent
  = UnknownEvent
  { unknownEventType :: !Text
  , unknownEventField1 :: !Text
  , unknownEventField2 :: !Text
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
  , startEventFieldingPosition :: !FieldingPosition
  } deriving (Eq, Show, Generic)

data SubEvent
  = SubEvent
  { subEventPlayer :: !Text
  , subEventPlayerName :: !Text
  , subEventPlayerHome :: !HomeOrAway
  , subEventBattingPosition :: !BattingOrderPosition
  , subEventFieldingPosition :: !FieldingPosition
  } deriving (Eq, Show, Generic)

data DataEvent
  = DataEvent
  { dataEventKey :: !Text
  , dataEventPlayerId :: !Text
  , dataEventValue :: !Int
  } deriving (Eq, Show, Generic)

data PlayEvent
  = PlayEvent
  { playEventInning :: !Int
  , playEventHomeOrAway :: !HomeOrAway
  , playEventPlayerId :: !Text
  , playEventCount :: !Int
  , playEventPitchSequence :: !Text
  , playEventResult :: !Play
  } deriving (Eq, Show, Generic)

makeClassy_ ''PlayEvent
