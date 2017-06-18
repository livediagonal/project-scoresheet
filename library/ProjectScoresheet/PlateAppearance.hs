{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module ProjectScoresheet.PlateAppearance where

import ClassyPrelude hiding (try)
import Control.Lens
import ProjectScoresheet.BaseballTypes
import ProjectScoresheet.EventTypes
import ProjectScoresheet.PlayResult

data PlateAppearance
  = PlateAppearance
  { plateAppearanceBatterId :: !Text
  , plateAppearanceOutsFromBatter :: ![Out]
  , plateAppearanceOutsFromRunners :: ![Out]
  , plateAppearanceAction :: !PlayAction
  , plateAppearanceAnnotations :: ![PlayDescriptor]
  , plateAppearanceMovements :: ![PlayMovement]
  } deriving (Eq, Show)

makeClassy_ ''PlateAppearance

initialPlateAppearance :: Text -> PlateAppearance
initialPlateAppearance batterId = PlateAppearance
  { plateAppearanceBatterId = batterId
  , plateAppearanceOutsFromBatter = []
  , plateAppearanceOutsFromRunners = []
  , plateAppearanceAction = NoPlay Nothing
  , plateAppearanceAnnotations = []
  , plateAppearanceMovements = []
  }

convertEventToPlateAppearance :: PlayEvent -> PlateAppearance
convertEventToPlateAppearance PlayEvent{..} =
  (initialPlateAppearance playEventPlayerId)
    { plateAppearanceAction = playResultAction playEventResult
    , plateAppearanceAnnotations = playResultDescriptors playEventResult
    , plateAppearanceMovements = playResultMovements playEventResult
    }

convertToPlateAppearance :: Text -> PlayResult -> PlateAppearance
convertToPlateAppearance batterId (PlayResult action descriptors movements) =
  initialPlateAppearance batterId
  & _plateAppearanceAction .~ action
  & _plateAppearanceAnnotations .~ descriptors
  & _plateAppearanceMovements .~ movements

isHit :: PlateAppearance -> Bool
isHit PlateAppearance{..} =
  case plateAppearanceAction of
    Hit _ _ -> True
    _ -> False

isRBI :: PlayMovement -> Bool
isRBI (PlayMovement _ HomePlate True) = True
isRBI _ = False

fromBool :: Bool -> Int
fromBool False = 0
fromBool True = 1

numRBI :: PlateAppearance -> Int
numRBI pa@PlateAppearance{..} =
  if isWildPitch pa
  then 0
  else length $ filter isRBI plateAppearanceMovements

isHomeRun :: PlateAppearance -> Bool
isHomeRun PlateAppearance{..} =
  case plateAppearanceAction of
    Hit HomePlate _ -> True
    _ -> False

isAtBat :: PlateAppearance -> Bool
isAtBat pa@PlateAppearance{..} =
  case plateAppearanceAction of
    Walk _ -> False
    HitByPitch -> False
    WildPitch -> False
    NoPlay _ -> False
    Other _ -> False
    StolenBase _ -> False
    Outs _ -> not $ isSacrifice pa
    _ -> True

isWalk :: PlateAppearance -> Bool
isWalk PlateAppearance{..} =
  case plateAppearanceAction of
    Walk _ -> True
    _ -> False

isStrikeout :: PlateAppearance -> Bool
isStrikeout PlateAppearance{..} =
  case plateAppearanceAction of
    Outs outs -> any isStrikeoutOut outs
    _ -> False

isStrikeoutOut :: Out -> Bool
isStrikeoutOut out =
  case out of
    Strikeout _ -> True
    _ -> False

isSacrifice :: PlateAppearance -> Bool
isSacrifice PlateAppearance{..} = any isSacrificeDescriptor plateAppearanceAnnotations

isSacrificeDescriptor :: PlayDescriptor -> Bool
isSacrificeDescriptor SacrificeFly = True
isSacrificeDescriptor SacrificeBunt = True
isSacrificeDescriptor _ = False

isWildPitch :: PlateAppearance -> Bool
isWildPitch PlateAppearance{..} =
  case plateAppearanceAction of
    WildPitch -> True
    _ -> False
