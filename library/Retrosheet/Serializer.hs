{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Retrosheet.Serializer
  ( toCsv
  ) where

import ClassyPrelude
import qualified Data.ByteString.Lazy as BL
import Data.Csv

import Baseball.BaseballTypes
import Baseball.Event
import Baseball.Game.GameEvent
import Baseball.Game.GameState
import Baseball.Game.FrameState

toCsv :: [GameEvent] -> BL.ByteString
toCsv ges = encodeWith (defaultEncodeOptions {encQuoting = QuoteNone}) (map SerializableGameEvent ges)

newtype SerializableGameEvent = SerializableGameEvent GameEvent

instance ToRecord SerializableGameEvent where
  toRecord (SerializableGameEvent (GameEvent (PlayEvent play) gs fs)) = record $ fullList play gs fs

  toRecord (SerializableGameEvent (GameEvent (SubstitutionEvent Substitution{..}) gs fs)) = record
    [ toField (gameStateInning gs)
    , toField (frameStateOuts fs)
    , toField (show subPlayer)
    ]

fullList :: Play -> GameState -> FrameState -> [ByteString]
fullList p@Play{..} gs@GameState{..} fs@FrameState{..} =
    [ -- gameStateId
    -- , gameStateVisitingTeam
      toField gameStateInning
    -- , gameStateBattingTeam
    , toField frameStateOuts
    -- , playBalls
    -- , playStrikes
    -- , playPitchSequence
    -- , gameStateHomeScore
    -- , gameStateAwayScore
    -- , playBatter
    -- , playBatterHand
    -- , playResBatter
    -- , playResBatterHand
    -- , playPitcher
    -- , playPitcherHand
    -- , playResPitcher
    -- , playResPitcherHand
    , toField $ playerAtPosition Catcher gs
    , toField $ playerAtPosition FirstBaseman gs
    , toField $ playerAtPosition SecondBaseman gs
    , toField $ playerAtPosition ThirdBaseman gs
    , toField $ playerAtPosition ShortStop gs
    , toField $ playerAtPosition LeftFielder gs
    , toField $ playerAtPosition CenterFielder gs
    , toField $ playerAtPosition RightFielder gs
    -- , runnerOnBase FirstBase fs
    -- , runnerOnBase SecondBase fs
    -- , runnerOnBase ThirdBase fs
    -- , playEventText
    -- , isLeadoff p
    -- , isPinchHit p
    -- , defensivePosition
    -- , toField $ isAtBat p
    -- , hitValue p
    -- , shflag
    -- , sfflag
    -- , numOuts p
    -- , isDoublePlay p
    -- , isTriplePlay p
    -- , numRBIs p
    , toField $ SerializableBool (isWildPitch p)
    , toField $ SerializableBool (isPassedBall p)
    -- , fieldedBy p
    -- , battedBallType
    -- , isBunt p
    -- , isFoul p,
    -- , hitLocation p
    -- , numErrors p
    -- , firstErrorPlayer p
    -- , firstErrorType p
    -- , secondErrorPlayer p
    -- , secondErrorType p
    -- , thirdErrorPlayer p
    -- , thirdErrorType p
    -- , batterDest p
    -- , runnerAdvancement FirstBase
    -- , runnerAdvancement SecondBase
    -- , runnerAdvancement ThirdBase
    -- , playOnBatter
    -- , playOnFirstRunner
    -- , playOnSecondRunner
    -- , playOnThirdRunner
    -- , stolenBaseForRunnerOn1st
    -- , stolenBaseForRunnerOn2nd
    -- , stolenBaseForRunnerOn3rd
    -- , caughtStealingForRunnerOn1st
    -- , caughtStealingForRunnerOn2nd
    -- , caughtStealingForRunnerOn3rd
    -- , pickoffRunnerOnFirst
    -- , pickoffRunnerOnSecond
    -- , pickoffRunnerOnThird
    -- , responsiblePitcherForRunnerOn FirstBase
    -- , responsiblePitcherForRunnerOn SecondBase
    -- , responsiblePitcherForRunnerOn ThirdBase
    -- , newGameFlag
    -- , endGameFlag
    -- , pinchRunnerOn FirstBase
    -- , pinchRunnerOn SecondBase
    -- , pinchRunnerOn ThirdBase
    -- , runnerRemovedForPinchRunnerOn FirstBase
    -- , runnerRemovedForPinchRunnerOn SecondBase
    -- , runnerRemovedForPinchRunnerOn ThirdBase
    -- , batterRemovedFroPinchHitten
    -- , positionOfRemovedBatter
    -- , firstPutoutFielder
    -- , secondPutoutFielder
    -- , thirdPutoutFielder
    -- , firstAssistFielder
    -- , secondAssistFielder
    -- , thirdAssistFielder
    -- , fourthAssistFielder
    -- , fifthAssistFielder
    -- , eventNum
    ]

newtype SerializableBool = SerializableBool Bool

instance ToField SerializableBool where
  toField (SerializableBool True)   = "T"
  toField (SerializableBool False)  = "F"
