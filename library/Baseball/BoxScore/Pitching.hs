{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Baseball.BoxScore.Pitching
  ( Pitching
  , initialPitching
  , addPlayerToPitching
  , addPlayToPitching
  , prettyPrintPitching
  ) where

import ClassyPrelude hiding (replicate)
import Control.Lens

import Baseball.BaseballTypes
import Baseball.Game.GameState
import Baseball.Play
import Retrosheet.Events
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.Text (replicate)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap

data Pitching
  = Pitching
  { pitchingStats :: !(InsOrdHashMap Text PitchingLine)
  } deriving (Eq, Show)

data PitchingLine
  = PitchingLine
  { pitchingLinePlayer :: !Text
  , pitchingLineStrikeouts :: !Int
  , pitchingLineWalks :: !Int
  , pitchingLineHits :: !Int
  } deriving (Eq, Show, Generic)

makeClassy_ ''Pitching
makeClassy_ ''PitchingLine

initialPitching :: Pitching
initialPitching = Pitching InsOrdHashMap.empty

initialPitchingLine :: Text -> PitchingLine
initialPitchingLine player = PitchingLine player 0 0 0

addPlayerToPitching :: Text -> FieldingPosition -> Pitching -> Pitching
addPlayerToPitching player Pitcher = _pitchingStats %~ (at player ?~ initialPitchingLine player)
addPlayerToPitching _ _ = id

addPlayToPitching :: PlayEvent -> GameState -> Pitching -> Pitching
addPlayToPitching PlayEvent{..} GameState{..} =
  let
    pitcherId = case playEventHomeOrAway of
      Away -> gameStateHomeLineup HashMap.! Pitcher
      Home -> gameStateAwayLineup HashMap.! Pitcher
  in pitching %~ (if isHit playEventResult then addHitToPitcher pitcherId else id)

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

prettyPrintPitching :: Pitching -> Text
prettyPrintPitching Pitching{..} = unlines
  [ "-------------------------------------------"
  , "Pitchers        IP   H   R   ER   W  SO  HR"
  , "-------------------------------------------"
  , prettyPrintPitchingLines pitchingStats
  ]

prettyPrintPitchingLines :: InsOrdHashMap Text PitchingLine -> Text
prettyPrintPitchingLines pls = unlines $ map prettyPrintPitchingLine (InsOrdHashMap.elems pls)

prettyPrintPitchingLine :: PitchingLine -> Text
prettyPrintPitchingLine PitchingLine{..} = pitchingLinePlayer
  <> "          "
  <> prettyColumn (tshow pitchingLineHits)

prettyColumn :: Text -> Text
prettyColumn t = replicate (4 - length t) " " <>  t
