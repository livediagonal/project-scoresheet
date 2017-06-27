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
  { playActions :: ![PlayAction]
  , playDescriptors :: ![PlayDescriptor]
  , playMovements :: ![PlayMovement]
  } deriving (Eq, Show, Generic)

data PlayAction
  = RoutinePlay [FieldingPosition] (Maybe Base)
  | FieldersChoice [FieldingPosition]
  | Strikeout (Maybe Text)
  | Hit Base (Maybe [FieldingPosition])
  | StolenBase Base
  | CaughtStealing Base (Maybe [FieldingPosition])
  | Pickoff Base (Maybe [FieldingPosition])
  | WildPitch
  | PassedBall
  | DefensiveIndifference
  | Walk Bool
  | NoPlay
  | HitByPitch
  | Error FieldingPosition
  deriving (Eq, Show)

data PlayDescriptor
  = ForceOut
  | SacrificeFly
  | SacrificeBunt
  | OtherDescriptor !Text
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

isBatterAdvancedOnMovement :: PlayMovement -> Bool
isBatterAdvancedOnMovement (PlayMovement HomePlate _ True) = True
isBatterAdvancedOnMovement _ = False

isBatterOutOnAction :: PlayAction -> Bool
isBatterOutOnAction (Strikeout _) = True
isBatterOutOnAction (RoutinePlay _ Nothing) = True
isBatterOutOnAction _ = False

isBatterOut :: Play -> Bool
isBatterOut Play{..} =
  (any isBatterOutOnAction playActions || any isBatterOutOnMovement playMovements) &&
  not (any isBatterAdvancedOnMovement playMovements)

saturatePlayMovements :: Play -> Play
saturatePlayMovements p@Play{..} =
  let
    applyPlayAction pa = case pa of
      Walk _ -> over _playMovements (addPlayMovement (PlayMovement HomePlate FirstBase True))
      HitByPitch -> over _playMovements (addPlayMovement (PlayMovement HomePlate FirstBase True))
      Hit base _ -> over _playMovements (if isBatterOut p then id else addPlayMovement (PlayMovement HomePlate base True))
      StolenBase base -> over _playMovements (addPlayMovement (PlayMovement (baseBefore base) base True))
      CaughtStealing base _ -> over _playMovements (addPlayMovement (PlayMovement (baseBefore base) base False))
      Pickoff base _ -> over _playMovements (addPlayMovement (PlayMovement base base False))
      FieldersChoice _ -> over _playMovements (addPlayMovement (PlayMovement HomePlate FirstBase True))
      RoutinePlay _ (Just startingBase) -> over _playMovements (addPlayMovement (PlayMovement startingBase HomePlate False))
      _ -> id
  in
    foldr applyPlayAction p playActions
    & play %~ advanceBatterIfNotOut

advanceBatterIfNotOut :: Play -> Play
advanceBatterIfNotOut p =
  if isAtBat p
  then if isBatterOut p && not (isForceOut p)
    then p
    else p & _playMovements %~ addPlayMovement (PlayMovement HomePlate FirstBase True)
  else p

isHit :: Play -> Bool
isHit Play{..} =
  flip any playActions $ \a -> case a of
    Hit _ _ -> True
    _ -> False

isRBI :: PlayMovement -> Bool
isRBI (PlayMovement _ HomePlate True) = True
isRBI _ = False

fromBool :: Bool -> Int
fromBool False = 0
fromBool True = 1

numRBI :: Play -> Int
numRBI p@Play{..} =
  if isWildPitch p || isPassedBall p || isDefensiveIndifference p
  then 0
  else length $ filter isRBI playMovements

isHomeRun :: Play -> Bool
isHomeRun Play{..} =
  flip any playActions $ \a -> case a of
    Hit HomePlate _ -> True
    _ -> False

isAtBat :: Play -> Bool
isAtBat p@Play{..} =
  flip any playActions $ \a -> case a of
    Walk _ -> False
    HitByPitch -> False
    CaughtStealing _ _ -> False
    Pickoff _ _ -> False
    WildPitch -> False
    PassedBall -> False
    DefensiveIndifference -> False
    NoPlay -> False
    StolenBase _ -> False
    RoutinePlay _ _ -> not $ isSacrifice p
    _ -> True

isWalk :: Play -> Bool
isWalk Play{..} =
  flip any playActions $ \a -> case a of
    Walk _ -> True
    _ -> False

isStrikeout :: Play -> Bool
isStrikeout Play{..} =
  flip any playActions $ \a -> case a of
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
  flip any playActions $ \a -> case a of
    WildPitch -> True
    _ -> False

isPassedBall :: Play -> Bool
isPassedBall Play{..} =
  flip any playActions $ \a -> case a of
    PassedBall -> True
    _ -> False

isDefensiveIndifference :: Play -> Bool
isDefensiveIndifference Play{..} =
  flip any playActions $ \a -> case a of
    DefensiveIndifference -> True
    _ -> False
