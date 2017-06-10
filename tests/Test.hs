{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import ClassyPrelude
import Test.Hspec.Attoparsec
import ProjectScoresheet.BaseballTypes
import ProjectScoresheet.BoxScore
import ProjectScoresheet.EventExpander hiding (main)
import ProjectScoresheet.PlayResult
import Test.Tasty
import Test.Tasty.Hspec
import qualified Data.HashMap.Strict as HashMap

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

    it "should successfully parse HitByPitch" $
      "HP" `shouldParseAsPlay` HitByPitch

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

    it "should successfully parse routine out" $
      "53" `shouldParseAsPlay` Outs [RoutinePlay [ThirdBaseman, FirstBaseman]]

    it "should successfully parse annotated routine out" $
      "46(1)" `shouldParseAsPlay` Outs [RoutinePlay [SecondBaseman, ShortStop]]

    it "should successfully parse routine double play" $
      "46(1)3" `shouldParseAsPlay` Outs [RoutinePlay [SecondBaseman, ShortStop], RoutinePlay [FirstBaseman]]

  describe "parsePlayMovements" $ do
    let
      shouldParseMovements :: Text -> [PlayMovement] -> Expectation
      shouldParseMovements t pm = do
        t ~> many parsePlayMovement `shouldParse` pm
        leftover (t ~?> many parsePlayMovement) `shouldSatisfy` \str -> fromMaybe "" str == ""

    it "should successfully parse basic RBI" $
      ".3-H" `shouldParseMovements` [PlayMovement ThirdBase HomePlate True]

    it "should successfully parse out at home" $
      ".3XH" `shouldParseMovements` [PlayMovement ThirdBase HomePlate False]

    it "should successfully parse out with annotation" $
      ";2X3(65)" `shouldParseMovements` [PlayMovement SecondBase ThirdBase False]

    it "should successfully parse out multiple movements" $
      ".3-H;2X3(65)" `shouldParseMovements`
        [ PlayMovement ThirdBase HomePlate True
        , PlayMovement SecondBase ThirdBase False
        ]

  describe "box score smoke test" $ do
    let smokeScore = boxScoreFromFile "testgame.txt"

    it "should return correct stats" $
      smokeScore >>= (\BoxScore{..} -> do
        boxScoreCountsAtBats boxScoreStats `shouldBe` HashMap.fromList
          [ ("bogax001",4)
          , ("ortid001",5)
          , ("rickj001",4)
          , ("wietm001",4)
          , ("ramih003",3)
          , ("reimn001",3)
          , ("schoj001",3)
          , ("holtb002",2)
          , ("trumm001",5)
          , ("davic003",5)
          , ("younc004",2)
          , ("josec002",3)
          , ("shawt001",2)
          , ("flahr001",1)
          , ("machm001",3)
          , ("bettm001",5)
          , ("pedrd001",5)
          , ("swihb001",4)
          , ("hardj003",4)
          , ("bradj001",4)
          ]
        boxScoreCountsHits boxScoreStats `shouldBe` HashMap.fromList
          [ ("bogax001",1)
          , ("ortid001",2)
          , ("rickj001",1)
          , ("schoj001",1)
          , ("trumm001",1)
          , ("davic003",2)
          , ("josec002",1)
          , ("machm001",2)
          , ("bettm001",3)
          , ("pedrd001",2)
          , ("swihb001",2)
          , ("hardj003",1)
          , ("bradj001",1)
          ]
        boxScoreCountsRBI boxScoreStats `shouldBe` HashMap.fromList
          [ ("bogax001",1)
          , ("ortid001",1)
          , ("rickj001",0)
          , ("wietm001",0)
          , ("ramih003",1)
          , ("reimn001",0)
          , ("schoj001",1)
          , ("holtb002",0)
          , ("trumm001",3)
          , ("davic003",5)
          , ("younc004",0)
          , ("josec002",0)
          , ("shawt001",0)
          , ("flahr001",0)
          , ("machm001",0)
          , ("bettm001",2)
          , ("pedrd001",0)
          , ("swihb001",0)
          , ("hardj003",0)
          , ("bradj001",2)
          ]
        boxScoreCountsBB boxScoreStats `shouldBe` HashMap.fromList
          [ ("bogax001",1)
          , ("ortid001",0)
          , ("rickj001",1)
          , ("wietm001",0)
          , ("ramih003",1)
          , ("reimn001",0)
          , ("schoj001",1)
          , ("holtb002",2)
          , ("trumm001",0)
          , ("davic003",0)
          , ("younc004",0)
          , ("josec002",1)
          , ("shawt001",0)
          , ("flahr001",0)
          , ("machm001",1)
          , ("bettm001",0)
          , ("pedrd001",0)
          , ("swihb001",0)
          , ("hardj003",0)
          , ("bradj001",0)
          ]
        boxScoreCountsStrikeouts boxScoreStats `shouldBe` HashMap.fromList
          [ ("bogax001",1)
          , ("ortid001",1)
          , ("rickj001",2)
          , ("wietm001",4)
          , ("ramih003",2)
          , ("reimn001",2)
          , ("schoj001",1)
          , ("holtb002",0)
          , ("trumm001",1)
          , ("davic003",3)
          , ("younc004",1)
          , ("josec002",1)
          , ("shawt001",0)
          , ("flahr001",0)
          , ("machm001",0)
          , ("bettm001",1)
          , ("pedrd001",1)
          , ("swihb001",0)
          , ("hardj003",1)
          , ("bradj001",1)
          ]
        boxScoreCountsRuns boxScoreStats `shouldBe` HashMap.fromList
          [ ("bogax001",1)
          , ("rickj001",1)
          , ("holtb002",2)
          , ("trumm001",1)
          , ("davic003",2)
          , ("josec002",2)
          , ("machm001",2)
          , ("bettm001",2)
          , ("pedrd001",1)
          , ("swihb001",1)
          , ("hardj003",1)
          ]
      )
