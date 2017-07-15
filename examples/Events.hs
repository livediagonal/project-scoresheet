{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import ClassyPrelude hiding (putStr)
import Options.Applicative
import Data.Semigroup ((<>))
import Data.String.Class (putStr)

import Baseball.Game
import Retrosheet.Parser
import Retrosheet.Serializer

main :: IO ()
main = generateEvents =<< execParser opts
  where
    opts = info (parser <**> helper)
      ( fullDesc <>
        progDesc "bevent generates files suitable for use by database programs.\nEach record describes one event." <>
        header "Expanded event descriptor,  version 0 of 7/8/2017.\nType 'bevent -h' for help.\nCopyright 2017 Live Diagonal"
      )

type Year = String
type Team = String

data Options
  =  Options
  { optionsYear :: String
  , optionsTeam :: String
  }

parser :: Parser Options
parser = Options
  <$> strOption
    ( long "year" <> short 'y' <> help "Year to process (for teamyyyy and aaayyyy.ros)." )
  <*> strOption
    ( long "team" <> short 't' <> help "Team to process")

generateEvents :: Options -> IO ()
generateEvents Options{..} = gamesFromFilePath (eventFilePath optionsYear optionsTeam) >>= mapM_ printGameEvents

eventFilePath :: Year -> Team -> String
eventFilePath year team =
  "resources/retrosheet/event/regular/" <> year <> toUpper team <> ".EVA"

rosterFilePath :: Year -> Team -> String
rosterFilePath year team =
  "resources/retrosheet/event/regular/" <> toUpper team <> year <> ".ROS"

printGameEvents :: Game -> IO ()
printGameEvents Game{..} =
  putStr $ toCsv gameEvents