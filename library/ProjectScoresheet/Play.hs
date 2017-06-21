{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module ProjectScoresheet.Play where

import ClassyPrelude hiding (try)
import Control.Lens
import ProjectScoresheet.BaseballTypes

data Play
  = Play
  { playAction :: !PlayAction
  , playDescriptors :: ![PlayDescriptor]
  , playMovements :: ![PlayMovement]
  } deriving (Eq, Show, Generic)

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

data Out
  = RoutinePlay [FieldingPosition] (Maybe Base)
  | FieldersChoice [FieldingPosition]
  | Strikeout (Maybe Text)
  deriving (Eq, Show)

data PlayDescriptor
  = ForceOut
  | SacrificeFly
  | SacrificeBunt
  | OtherDescriptor Text
  deriving (Eq, Show)

data PlayMovement = PlayMovement Base Base Bool deriving (Eq, Show)

makeClassy_ ''Play

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

isForceOut :: Play -> Bool
isForceOut Play{..} = any isForceOutDescriptor playDescriptors

isBatterOutOnMovement :: PlayMovement -> Bool
isBatterOutOnMovement (PlayMovement HomePlate _ False) = True
isBatterOutOnMovement _ = False

isBatterOut :: Play -> Bool
isBatterOut Play{..} =
  any isBatterOutOnMovement playMovements

saturatePlayMovements :: Play -> Play
saturatePlayMovements pr@Play{..} =
  let
    pr' = case playAction of
      Walk _ -> over _playMovements (addPlayMovement (PlayMovement HomePlate FirstBase True)) pr
      HitByPitch -> over _playMovements (addPlayMovement (PlayMovement HomePlate FirstBase True)) pr
      Hit base _ -> over _playMovements (if isBatterOut pr then id else addPlayMovement (PlayMovement HomePlate base True)) pr
      StolenBase base -> over _playMovements (addPlayMovement (PlayMovement (baseBefore base) base True)) pr
      CaughtStealing base _ -> over _playMovements (addPlayMovement (PlayMovement (baseBefore base) base False)) pr
      Outs outs -> foldr saturateMovementsOnOut pr outs
        & _playMovements %~ if not (isBatterOutOnOuts outs) then addPlayMovement (PlayMovement HomePlate FirstBase True) else id
      _ -> pr
  in
    case isForceOut pr' of
      True -> over _playMovements (addPlayMovement (PlayMovement HomePlate FirstBase True)) pr'
      False -> pr'

batterOuts :: [Out] -> Int
batterOuts outs = length $ filter (\o -> case o of Strikeout _ -> True; RoutinePlay _ Nothing -> True; _ -> False) outs

isBatterOutOnOuts :: [Out] -> Bool
isBatterOutOnOuts = any isBatterOutOnOut

isBatterOutOnOut :: Out -> Bool
isBatterOutOnOut (Strikeout _) = True
isBatterOutOnOut (RoutinePlay _ Nothing) = True
isBatterOutOnOut _ = False

saturateMovementsOnOut :: Out -> Play -> Play
saturateMovementsOnOut (FieldersChoice _) pr = over _playMovements (addPlayMovement (PlayMovement HomePlate FirstBase True)) pr
saturateMovementsOnOut (RoutinePlay _ (Just startingBase)) pr = over _playMovements (addPlayMovement (PlayMovement startingBase HomePlate False)) pr
saturateMovementsOnOut _ pr = pr

isHit :: Play -> Bool
isHit Play{..} =
  case playAction of
    Hit _ _ -> True
    _ -> False

isRBI :: PlayMovement -> Bool
isRBI (PlayMovement _ HomePlate True) = True
isRBI _ = False

fromBool :: Bool -> Int
fromBool False = 0
fromBool True = 1

numRBI :: Play -> Int
numRBI pa@Play{..} =
  if isWildPitch pa
  then 0
  else length $ filter isRBI playMovements

isHomeRun :: Play -> Bool
isHomeRun Play{..} =
  case playAction of
    Hit HomePlate _ -> True
    _ -> False

isAtBat :: Play -> Bool
isAtBat pa@Play{..} =
  case playAction of
    Walk _ -> False
    HitByPitch -> False
    CaughtStealing _ _ -> False
    WildPitch -> False
    NoPlay _ -> False
    Other _ -> False
    StolenBase _ -> False
    Outs _ -> not $ isSacrifice pa
    _ -> True

isWalk :: Play -> Bool
isWalk Play{..} =
  case playAction of
    Walk _ -> True
    _ -> False

isStrikeout :: Play -> Bool
isStrikeout Play{..} =
  case playAction of
    Outs outs -> any isStrikeoutOut outs
    _ -> False

isStrikeoutOut :: Out -> Bool
isStrikeoutOut out =
  case out of
    Strikeout _ -> True
    _ -> False

isSacrifice :: Play -> Bool
isSacrifice Play{..} = any isSacrificeDescriptor playDescriptors

isSacrificeDescriptor :: PlayDescriptor -> Bool
isSacrificeDescriptor SacrificeFly = True
isSacrificeDescriptor SacrificeBunt = True
isSacrificeDescriptor _ = False

isWildPitch :: Play -> Bool
isWildPitch Play{..} =
  case playAction of
    WildPitch -> True
    _ -> False
