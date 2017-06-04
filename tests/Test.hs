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
spec = describe "PlayResult" $ do

  describe "parsePlayAction" $ do

    let
      shouldParseAsPlay :: Text -> PlayAction -> Expectation
      shouldParseAsPlay t pa = do
        t ~> parsePlayAction `shouldParse` pa
        leftover (t ~?> parsePlayAction) `shouldSatisfy` \str -> fromMaybe "" str == ""

    it "should successfully parse Strikeout" $
      "K" `shouldParseAsPlay` Outs [Strikeout Nothing]

    it "should successfully parse basic StolenBase" $
      "SB3" `shouldParseAsPlay` StolenBase ThirdBase

    it "should successfully parse basic Single" $
      "S7" `shouldParseAsPlay` Hit FirstBase (Just LeftFielder)

    it "should successfully parse basic Double" $
      "D8" `shouldParseAsPlay` Hit SecondBase (Just CenterFielder)

    it "should successfully parse ground-rule Double" $
      "DGR" `shouldParseAsPlay` Hit SecondBase Nothing

    it "should successfully parse basic Triple" $
      "T9" `shouldParseAsPlay` Hit ThirdBase (Just RightFielder)

    it "should successfully parse basic HomeRun" $
      "HR" `shouldParseAsPlay` Hit HomePlate Nothing

    it "should successfully parse basic inside-the-park HomeRun" $
      "HR7" `shouldParseAsPlay` Hit HomePlate (Just LeftFielder)

    it "should successfully parse fielders choice" $
      "FC6" `shouldParseAsPlay` Outs [FieldersChoice [ShortStop]]

    it "should successfully parse annotated routine out" $
      "46(1)" `shouldParseAsPlay` Outs [RoutinePlay [SecondBaseman, ShortStop]]

  describe "parsePlayMovements" $ do
    let
      shouldParseAsMovement :: Text -> PlayMovement -> Expectation
      shouldParseAsMovement t pm = do
        t ~> parsePlayMovement `shouldParse` pm
        leftover (t ~?> parsePlayMovement) `shouldSatisfy` \str -> fromMaybe "" str == ""

    it "should successfully parse basic RBI" $
      ".3-H" `shouldParseAsMovement` PlayMovement ThirdBase HomePlate True

    it "should successfully parse out at home" $
      ".3XH" `shouldParseAsMovement` PlayMovement ThirdBase HomePlate False

    it "should successfully parse out with annotation" $
      ";2X3(65)" `shouldParseAsMovement` PlayMovement SecondBase ThirdBase False
