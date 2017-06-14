{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import ClassyPrelude
import Test.Hspec.Attoparsec
import ProjectScoresheet.BaseballTypes
import ProjectScoresheet.BoxScore
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
      "53" `shouldParseAsPlay` Outs [RoutinePlay [ThirdBaseman, FirstBaseman] Nothing]

    it "should successfully parse annotated routine out" $
      "46(1)" `shouldParseAsPlay` Outs [RoutinePlay [SecondBaseman, ShortStop] (Just FirstBase)]

    it "should successfully parse routine double play" $
      "46(1)3" `shouldParseAsPlay` Outs [RoutinePlay [SecondBaseman, ShortStop] (Just FirstBase), RoutinePlay [FirstBaseman] Nothing]

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
      smokeScore >>= (\BoxScore{..} ->
        boxScoreStats `shouldBe` HashMap.fromList
          [ ("kimbc001",BattingLine {battingLinePlayerId = "kimbc001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("bogax001",BattingLine {battingLinePlayerId = "bogax001", battingLineAtBats = 4, battingLineRuns = 1, battingLineHits = 1, battingLineRBI = 1, battingLineWalks = 1, battingLineStrikeouts = 1, battingLineLOB = 0})
          , ("ortid001",BattingLine {battingLinePlayerId = "ortid001", battingLineAtBats = 5, battingLineRuns = 0, battingLineHits = 2, battingLineRBI = 1, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 2})
          , ("rickj001",BattingLine {battingLinePlayerId = "rickj001", battingLineAtBats = 4, battingLineRuns = 1, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 1, battingLineStrikeouts = 2, battingLineLOB = 2})
          , ("barnm001",BattingLine {battingLinePlayerId = "barnm001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("wietm001",BattingLine {battingLinePlayerId = "wietm001", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 4, battingLineLOB = 1})
          , ("gally001",BattingLine {battingLinePlayerId = "gally001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("britz001",BattingLine {battingLinePlayerId = "britz001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("givem001",BattingLine {battingLinePlayerId = "givem001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("jonea003",BattingLine {battingLinePlayerId = "jonea003", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("ramih003",BattingLine {battingLinePlayerId = "ramih003", battingLineAtBats = 3, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 1, battingLineWalks = 1, battingLineStrikeouts = 2, battingLineLOB = 1})
          , ("reimn001",BattingLine {battingLinePlayerId = "reimn001", battingLineAtBats = 3, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 2, battingLineLOB = 2})
          , ("schoj001",BattingLine {battingLinePlayerId = "schoj001", battingLineAtBats = 3, battingLineRuns = 0, battingLineHits = 1, battingLineRBI = 1, battingLineWalks = 1, battingLineStrikeouts = 1, battingLineLOB = 0})
          , ("mcfat001",BattingLine {battingLinePlayerId = "mcfat001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("holtb002",BattingLine {battingLinePlayerId = "holtb002", battingLineAtBats = 2, battingLineRuns = 2, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 2, battingLineStrikeouts = 0, battingLineLOB = 1})
          , ("trumm001",BattingLine {battingLinePlayerId = "trumm001", battingLineAtBats = 5, battingLineRuns = 1, battingLineHits = 1, battingLineRBI = 3, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 3})
          , ("davic003",BattingLine {battingLinePlayerId = "davic003", battingLineAtBats = 5, battingLineRuns = 2, battingLineHits = 2, battingLineRBI = 5, battingLineWalks = 0, battingLineStrikeouts = 3, battingLineLOB = 3})
          , ("younc004",BattingLine {battingLinePlayerId = "younc004", battingLineAtBats = 2, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 2})
          , ("josec002",BattingLine {battingLinePlayerId = "josec002", battingLineAtBats = 3, battingLineRuns = 2, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 1, battingLineStrikeouts = 1, battingLineLOB = 2})
          , ("tazaj001",BattingLine {battingLinePlayerId = "tazaj001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("pricd001",BattingLine {battingLinePlayerId = "pricd001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("shawt001",BattingLine {battingLinePlayerId = "shawt001", battingLineAtBats = 2, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 2})
          , ("flahr001",BattingLine {battingLinePlayerId = "flahr001", battingLineAtBats = 1, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("machm001",BattingLine {battingLinePlayerId = "machm001", battingLineAtBats = 3, battingLineRuns = 2, battingLineHits = 2, battingLineRBI = 0, battingLineWalks = 1, battingLineStrikeouts = 0, battingLineLOB = 1})
          , ("bettm001",BattingLine {battingLinePlayerId = "bettm001", battingLineAtBats = 5, battingLineRuns = 2, battingLineHits = 3, battingLineRBI = 2, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 2})
          , ("uehak001",BattingLine {battingLinePlayerId = "uehak001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("pedrd001",BattingLine {battingLinePlayerId = "pedrd001", battingLineAtBats = 5, battingLineRuns = 1, battingLineHits = 2, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 2})
          , ("swihb001",BattingLine {battingLinePlayerId = "swihb001", battingLineAtBats = 4, battingLineRuns = 1, battingLineHits = 2, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("hardj003",BattingLine {battingLinePlayerId = "hardj003", battingLineAtBats = 4, battingLineRuns = 1, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 0})
          , ("bradj001",BattingLine {battingLinePlayerId = "bradj001", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 1, battingLineRBI = 2, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 1})
          , ("bracb001",BattingLine {battingLinePlayerId = "bracb001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          ]
      )
