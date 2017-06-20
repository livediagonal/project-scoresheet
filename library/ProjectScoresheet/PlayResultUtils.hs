{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module ProjectScoresheet.PlayResultUtils where

import ClassyPrelude hiding (try)
import ProjectScoresheet.BaseballTypes
import ProjectScoresheet.PlayResult

isHit :: PlayResult -> Bool
isHit PlayResult{..} =
  case playResultAction of
    Hit _ _ -> True
    _ -> False

isRBI :: PlayMovement -> Bool
isRBI (PlayMovement _ HomePlate True) = True
isRBI _ = False

fromBool :: Bool -> Int
fromBool False = 0
fromBool True = 1

numRBI :: PlayResult -> Int
numRBI pa@PlayResult{..} =
  if isWildPitch pa
  then 0
  else length $ filter isRBI playResultMovements

isHomeRun :: PlayResult -> Bool
isHomeRun PlayResult{..} =
  case playResultAction of
    Hit HomePlate _ -> True
    _ -> False

isAtBat :: PlayResult -> Bool
isAtBat pa@PlayResult{..} =
  case playResultAction of
    Walk _ -> False
    HitByPitch -> False
    CaughtStealing _ _ -> False
    WildPitch -> False
    NoPlay _ -> False
    Other _ -> False
    StolenBase _ -> False
    Outs _ -> not $ isSacrifice pa
    _ -> True

isWalk :: PlayResult -> Bool
isWalk PlayResult{..} =
  case playResultAction of
    Walk _ -> True
    _ -> False

isStrikeout :: PlayResult -> Bool
isStrikeout PlayResult{..} =
  case playResultAction of
    Outs outs -> any isStrikeoutOut outs
    _ -> False

isStrikeoutOut :: Out -> Bool
isStrikeoutOut out =
  case out of
    Strikeout _ -> True
    _ -> False

isSacrifice :: PlayResult -> Bool
isSacrifice PlayResult{..} = any isSacrificeDescriptor playResultDescriptors

isSacrificeDescriptor :: PlayDescriptor -> Bool
isSacrificeDescriptor SacrificeFly = True
isSacrificeDescriptor SacrificeBunt = True
isSacrificeDescriptor _ = False

isWildPitch :: PlayResult -> Bool
isWildPitch PlayResult{..} =
  case playResultAction of
    WildPitch -> True
    _ -> False
