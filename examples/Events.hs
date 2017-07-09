{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import ClassyPrelude
import Options.Applicative
import Data.Semigroup ((<>))

main :: IO ()
main = generateEvents =<< execParser opts
  where
    opts = info (parser <**> helper)
      ( fullDesc <>
        progDesc "bevent generates files suitable for use by database programs.\nEach record describes one event." <>
        header "Expanded event descriptor,  version 0 of 7/8/2017.\nType 'bevent -h' for help.\nCopyright 2017 Live Diagonal" 
      )

data Flag
  =  Flag 
  { flagYear :: String
  }

parser :: Parser Flag
parser = Flag
  <$> strOption 
    ( long "year" <> short 'y' <> help "Year to process (for teamyyyy and aaayyyy.ros)." )

generateEvents :: Flag -> IO ()
generateEvents (Flag s) = putStrLn $ tshow s    