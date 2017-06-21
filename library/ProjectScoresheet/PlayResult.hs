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
import ProjectScoresheet.BaseballTypes

data Out
  = RoutinePlay [FieldingPosition] (Maybe Base)
  | FieldersChoice [FieldingPosition]
  | Strikeout (Maybe Text)
  deriving (Eq, Show)

data PlayAction
  = Outs [Out]
  | Hit Base (Maybe FieldingPosition)
  | StolenBase Base
  | CaughtStealing Base (Maybe [FieldingPosition])
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

addPlayMovement :: PlayMovement -> [PlayMovement] -> [PlayMovement]
addPlayMovement pm@(PlayMovement startBase _ _) pms =
  case any (\(PlayMovement existingStartBase _ _) -> startBase == existingStartBase) pms of
    True -> pms
    False -> pms ++ [pm]

baseBefore :: Base -> Base
baseBefore FirstBase = HomePlate
baseBefore SecondBase = FirstBase
baseBefore ThirdBase = SecondBase
baseBefore HomePlate = ThirdBase

isForceOutDescriptor :: PlayDescriptor -> Bool
isForceOutDescriptor ForceOut = True
isForceOutDescriptor _ = False

isForceOut :: PlayResult -> Bool
isForceOut PlayResult{..} = any isForceOutDescriptor playResultDescriptors

isBatterOutOnMovement :: PlayMovement -> Bool
isBatterOutOnMovement (PlayMovement HomePlate _ False) = True
isBatterOutOnMovement _ = False

isBatterOut :: PlayResult -> Bool
isBatterOut PlayResult{..} =
  any isBatterOutOnMovement playResultMovements

saturatePlayMovements :: PlayResult -> PlayResult
saturatePlayMovements pr@PlayResult{..} =
  let
    pr' = case playResultAction of
      Walk _ -> over _playResultMovements (addPlayMovement (PlayMovement HomePlate FirstBase True)) pr
      HitByPitch -> over _playResultMovements (addPlayMovement (PlayMovement HomePlate FirstBase True)) pr
      Hit base _ -> over _playResultMovements (if isBatterOut pr then id else addPlayMovement (PlayMovement HomePlate base True)) pr
      StolenBase base -> over _playResultMovements (addPlayMovement (PlayMovement (baseBefore base) base True)) pr
      CaughtStealing base _ -> over _playResultMovements (addPlayMovement (PlayMovement (baseBefore base) base False)) pr
      Outs outs -> foldr saturateMovementsOnOut pr outs
        & _playResultMovements %~ if not (isBatterOutOnOuts outs) then addPlayMovement (PlayMovement HomePlate FirstBase True) else id
      _ -> pr
  in
    case isForceOut pr' of
      True -> over _playResultMovements (addPlayMovement (PlayMovement HomePlate FirstBase True)) pr'
      False -> pr'

isBatterOutOnOuts :: [Out] -> Bool
isBatterOutOnOuts = any isBatterOutOnOut

isBatterOutOnOut :: Out -> Bool
isBatterOutOnOut (Strikeout _) = True
isBatterOutOnOut (RoutinePlay _ Nothing) = True
isBatterOutOnOut _ = False

saturateMovementsOnOut :: Out -> PlayResult -> PlayResult
saturateMovementsOnOut (FieldersChoice _) pr = over _playResultMovements (addPlayMovement (PlayMovement HomePlate FirstBase True)) pr
saturateMovementsOnOut (RoutinePlay _ (Just startingBase)) pr = over _playResultMovements (addPlayMovement (PlayMovement startingBase HomePlate False)) pr
saturateMovementsOnOut _ pr = pr
