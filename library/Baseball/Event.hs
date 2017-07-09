{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Baseball.Event
 ( Event(..)
 , Substitution(..)
 , Play(..)
 , PlayAction(..)
 , PlayDescriptor(..)
 , PlayMovement(..)
 , PlayMovementDescriptor(..)
 , addPlayMovement
 , baseBefore
 , saturatePlayMovements
 , advanceBatterIfNotOut
 , fromBool
 , isForceOutDescriptor
 , isForceOut
 , isBatterOutOnMovement
 , isBatterAdvancedOnMovement
 , isBatterOutOnAction
 , isBatterOut
 , isHit
 , isRBI
 , isSacrifice
 , isStrikeout
 , isWalk
 , isSacrificeDescriptor
 , isWildPitch
 , isPassedBall
 , isHomeRun
 , isDefensiveIndifference
 , isAtBat
 , isMovementEarned
 , isRunOnMovement
 , numOuts
 , numRBI
 , numRuns
 , if'
 , (?)
 ) where

import ClassyPrelude hiding (try)
import Control.Lens

import Baseball.BaseballTypes

data Event
  = PlayEvent Play
  | SubstitutionEvent Substitution
  deriving (Eq)

data Substitution
  = Substitution
  { subPlayer :: !Text
  , subTeam :: !HomeOrAway
  , subBattingPosition :: !BattingOrderPosition
  , subFieldingPosition :: !FieldingPosition
  } deriving (Eq)

data Play
  = Play
  { playPlayer :: !Text
  , playActions :: ![PlayAction]
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
  | Pickoff Bool Base Bool (Maybe [FieldingPosition])
  | WildPitch
  | PassedBall
  | DefensiveIndifference
  | Error FieldingPosition
  | ErrorFoulFly FieldingPosition
  | Walk Bool
  | NoPlay
  | Balk
  | HitByPitch
  deriving (Eq, Show)

data PlayDescriptor
  = ForceOut
  | SacrificeFly
  | SacrificeBunt
  | OtherDescriptor !Text
  deriving (Eq, Show)

data PlayMovement
  = PlayMovement
  { playMovementStartBase :: Base
  , playMovementEndBase :: Base
  , playMovementIsSuccess :: Bool
  , playMovementDescriptors :: [PlayMovementDescriptor]
  } deriving (Eq, Show)

data PlayMovementDescriptor
  = PlayMovementNoRBI
  | PlayMovementUnearned
  | PlayMovementFieldingSequence Bool [FieldingPosition]
  deriving (Eq, Show)

instance Ord PlayMovement where
  (PlayMovement sb1 _ _ _) `compare` (PlayMovement sb2 _ _ _) = sb1 `compare` sb2

makeClassy_ ''Play

addPlayMovement :: PlayMovement -> [PlayMovement] -> [PlayMovement]
addPlayMovement pm@(PlayMovement startBase _ _ _) pms =
  case any (\PlayMovement{..} -> playMovementStartBase == startBase) pms of
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

isMovementEarned :: PlayMovement -> Bool
isMovementEarned PlayMovement{..} = not $ any (\desc -> case desc of PlayMovementUnearned -> True; _ -> False) playMovementDescriptors

isBatterOutOnMovement :: PlayMovement -> Bool
isBatterOutOnMovement (PlayMovement HomePlate _ False _) = True
isBatterOutOnMovement _ = False

isBatterAdvancedOnMovement :: PlayMovement -> Bool
isBatterAdvancedOnMovement (PlayMovement HomePlate _ True _) = True
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
      Walk _ -> over _playMovements (addPlayMovement (PlayMovement HomePlate FirstBase True []))
      HitByPitch -> over _playMovements (addPlayMovement (PlayMovement HomePlate FirstBase True []))
      Hit base _ -> over _playMovements (if isBatterOut p then id else addPlayMovement (PlayMovement HomePlate base True []))
      StolenBase base -> over _playMovements (addPlayMovement (PlayMovement (baseBefore base) base True [PlayMovementNoRBI]))
      CaughtStealing base _ -> over _playMovements (addPlayMovement (PlayMovement (baseBefore base) base False []))
      Pickoff _ base False _ -> over _playMovements (addPlayMovement (PlayMovement base base False []))
      FieldersChoice _ -> over _playMovements (addPlayMovement (PlayMovement HomePlate FirstBase True []))
      RoutinePlay _ (Just startingBase) -> over _playMovements (addPlayMovement (PlayMovement startingBase HomePlate False []))
      _ -> id
  in
    foldr applyPlayAction p playActions
    & play %~ advanceBatterIfNotOut
    & _playMovements %~ sortBy (flip compare)

advanceBatterIfNotOut :: Play -> Play
advanceBatterIfNotOut p =
  if isAtBat p
  then if isBatterOut p && not (isForceOut p)
    then p
    else p & _playMovements %~ addPlayMovement (PlayMovement HomePlate FirstBase True [])
  else p

isHit :: Play -> Bool
isHit Play{..} =
  flip any playActions $ \a -> case a of
    Hit _ _ -> True
    _ -> False

isRBI :: PlayMovement -> Bool
isRBI (PlayMovement _ HomePlate True descriptors) = not $ any (\d -> case d of PlayMovementNoRBI -> True; _ -> False) descriptors
isRBI _ = False

fromBool :: Bool -> Int
fromBool False = 0
fromBool True = 1

isRunOnMovement :: PlayMovement -> Bool
isRunOnMovement (PlayMovement _ HomePlate True _) = True
isRunOnMovement _ = False

isOutOnMovement :: PlayMovement -> Bool
isOutOnMovement (PlayMovement _ _ False _) = True
isOutOnMovement _ = False

isRunnerMovement :: PlayMovement -> Bool
isRunnerMovement (PlayMovement HomePlate _ _ _) = False
isRunnerMovement _ = True

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

infixl 1 ?
(?) :: Bool -> a -> a -> a
(?) = if'

numRuns :: Play -> Int
numRuns p@Play{..} = length (filter isRunOnMovement playMovements) + (isHomeRun p ? 1 $ 0)

numOuts :: Play -> Int
numOuts p@Play{..} = length (filter (\move -> isOutOnMovement move && isRunnerMovement move) playMovements) + (isBatterOut p ? 1 $ 0)

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
    Pickoff{} -> False
    WildPitch -> False
    PassedBall -> False
    DefensiveIndifference -> False
    NoPlay -> False
    StolenBase _ -> False
    RoutinePlay _ _ -> not $ isSacrifice p
    ErrorFoulFly _ -> False
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
