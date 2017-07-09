{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Baseball.BoxScore.Pitching
  ( Pitching(..)
  , PitchingLine(..)
  , initialPitching
  , addPlayerToPitching
  , addPlayToPitching
  , prettyPrintPitching
  ) where

import ClassyPrelude hiding (replicate)
import Control.Lens

import Baseball.BaseballTypes
import Baseball.Game.FrameState
import Baseball.Game.GameState
import Baseball.Event
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.Text (replicate)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Generics.Deriving.Monoid hiding ((<>))

data Pitching
  = Pitching
  { pitchingStats :: !(InsOrdHashMap Text PitchingLine)
  } deriving (Eq, Show)

data PitchingLine
  = PitchingLine
  { pitchingLinePlayerId :: !Text
  , pitchingLineStrikeouts :: !Int
  , pitchingLineWalks :: !Int
  , pitchingLineHits :: !Int
  , pitchingLineHomeRuns :: !Int
  , pitchingLineRuns :: !Int
  , pitchingLineEarnedRuns :: !Int
  , pitchingLineOutsPitched :: !Int
  } deriving (Eq, Show, Generic)

makeClassy_ ''Pitching
makeClassy_ ''PitchingLine

instance Monoid Int where
  mempty  = 0
  mappend = (+)

instance Monoid PitchingLine where
  mempty = initialPitchingLine "Total"
  mappend = mappenddefault

initialPitching :: Pitching
initialPitching = Pitching InsOrdHashMap.empty

initialPitchingLine :: Text -> PitchingLine
initialPitchingLine player = PitchingLine player 0 0 0 0 0 0 0

addPlayerToPitching :: Text -> FieldingPosition -> Pitching -> Pitching
addPlayerToPitching player Pitcher p =
  InsOrdHashMap.member player (pitchingStats p) ? p $ p & _pitchingStats %~ (at player ?~ initialPitchingLine player)
addPlayerToPitching _ _ p = p

addPlayToPitching :: Play -> GameState -> FrameState -> Pitching -> Pitching
addPlayToPitching play gs@GameState{..} fs p =
  let
    pitcherId = currentPitcherId gs
    batter = BaseRunner (playPlayer play) pitcherId
  in
    p &
    pitching %~ (if isHit play then addHitToPitcher pitcherId else id) &
    pitching %~ (if isHomeRun play then addHomeRunToPitcher pitcherId else id) &
    pitching %~ (if isStrikeout play then addStrikeoutToPitcher pitcherId else id) &
    pitching %~ (if isWalk play then addWalkToPitcher pitcherId else id) &
    pitching %~ addOutsPitchedToPitcher pitcherId (numOuts play) &
    pitching %~ addRunsToPitchers fs batter play

addToPitcher
  :: Text
  -> Int
  -> ASetter PitchingLine PitchingLine Int Int
  -> Pitching
  -> Pitching
addToPitcher pitcher num stat =
  _pitchingStats . at pitcher %~ map (stat %~ (+num))

addOneToPitcher
  :: Text
  -> ASetter PitchingLine PitchingLine Int Int
  -> Pitching
  -> Pitching
addOneToPitcher pitcher stat = addToPitcher pitcher 1 stat

addHitToPitcher :: Text -> Pitching -> Pitching
addHitToPitcher pitcher = addOneToPitcher pitcher _pitchingLineHits

addHomeRunToPitcher :: Text -> Pitching -> Pitching
addHomeRunToPitcher pitcher = addOneToPitcher pitcher _pitchingLineHomeRuns

addStrikeoutToPitcher :: Text -> Pitching -> Pitching
addStrikeoutToPitcher pitcher = addOneToPitcher pitcher _pitchingLineStrikeouts

addWalkToPitcher :: Text -> Pitching -> Pitching
addWalkToPitcher pitcher = addOneToPitcher pitcher _pitchingLineWalks

addRunToPitcher :: Text -> Pitching -> Pitching
addRunToPitcher pitcher = addOneToPitcher pitcher _pitchingLineRuns

addEarnedRunToPitcher :: Text -> Pitching -> Pitching
addEarnedRunToPitcher pitcher = addOneToPitcher pitcher _pitchingLineEarnedRuns

chargePitcherForMovement :: FrameState -> BaseRunner -> PlayMovement -> Pitching -> Pitching
chargePitcherForMovement fs batter move@(PlayMovement startBase HomePlate True _) =
  let
    Just BaseRunner{..} = runnerOnBaseOrBatter batter startBase fs
  in
    (isMovementEarned move ? addEarnedRunToPitcher baseRunnerResponsiblePitcherId $ id) .
    addRunToPitcher baseRunnerResponsiblePitcherId
chargePitcherForMovement _ _ _ = id

addOutsPitchedToPitcher :: Text -> Int -> Pitching -> Pitching
addOutsPitchedToPitcher pitcher outs = addToPitcher pitcher outs _pitchingLineOutsPitched

addRunsToPitchers :: FrameState -> BaseRunner -> Play -> Pitching -> Pitching
addRunsToPitchers fs batter Play{..} p = foldr (chargePitcherForMovement fs batter) p playMovements

prettyPrintPitching :: Pitching -> Text
prettyPrintPitching Pitching{..} = unlines
  [ "-------------------------------------------"
  , "Pitchers        IP   H   R   ER   W  SO  HR"
  , "-------------------------------------------"
  , prettyPrintPitchingLines pitchingStats
  , "-------------------------------------------"
  , prettyPrintPitchingLine $ set _pitchingLinePlayerId "Total:  " $ mconcat (InsOrdHashMap.elems pitchingStats)
  ]

prettyPrintPitchingLines :: InsOrdHashMap Text PitchingLine -> Text
prettyPrintPitchingLines pls = unlines $ map prettyPrintPitchingLine (InsOrdHashMap.elems pls)

prettyPrintPitchingLine :: PitchingLine -> Text
prettyPrintPitchingLine PitchingLine{..} = pitchingLinePlayerId
  <> "      "
  <> prettyColumn (tshow (pitchingLineOutsPitched `div` 3) <> "." <> tshow (pitchingLineOutsPitched `mod` 3))
  <> prettyColumn (tshow pitchingLineHits)
  <> prettyColumn (tshow pitchingLineRuns)
  <> prettyColumn (tshow pitchingLineEarnedRuns)
  <> prettyColumn (tshow pitchingLineWalks)
  <> prettyColumn (tshow pitchingLineStrikeouts)
  <> prettyColumn (tshow pitchingLineHomeRuns)

prettyColumn :: Text -> Text
prettyColumn t = replicate (4 - length t) " " <>  t
