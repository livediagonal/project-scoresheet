{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import ClassyPrelude
import Test.Hspec.Attoparsec
import ProjectScoresheet.BaseballTypes
import ProjectScoresheet.PlayResult
import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
  tests <- testSpec "play-result" (parallel spec)
  defaultMain tests

spec :: Spec
spec = describe "PlayResult" $

  describe "parsePlayAction" $ do

    it "should successfully parse Strikeout" $
      ("K" :: Text) ~> parsePlayAction `shouldParse` Outs [Strikeout Nothing]

    it "should successfully parse basic Single" $
      ("S7" :: Text) ~> parsePlayAction `shouldParse` Hit FirstBase (Just LeftFielder)

    it "should successfully parse basic Double" $
      ("D8" :: Text) ~> parsePlayAction `shouldParse` Hit SecondBase (Just CenterFielder)

    it "should successfully parse basic Triple" $
      ("T9" :: Text) ~> parsePlayAction `shouldParse` Hit ThirdBase (Just RightFielder)

    it "should successfully parse basic HomeRun" $
      ("HR" :: Text) ~> parsePlayAction `shouldParse` Hit HomePlate Nothing

    it "should successfully parse basic inside-the-park HomeRun" $
      ("HR7" :: Text) ~> parsePlayAction `shouldParse` Hit HomePlate (Just LeftFielder)
