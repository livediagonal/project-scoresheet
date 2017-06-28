{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import Test.Hspec.Attoparsec
import Test.Tasty
import Test.Tasty.Hspec

import Baseball.BaseballTypes
import Baseball.BoxScore
import Baseball.BoxScore.Batting
import Baseball.Game (gamesFromFilePath)
import Baseball.Play
import Retrosheet.Parser

main :: IO ()
main = do
  tests <- testSpec "play-result" (parallel spec)
  defaultMain tests

spec :: Spec
spec = describe "Play" $ do

  describe "parsePlay" $ do

    it "should successfully parse non-RBI wild pitch" $ do
      let
        res = ("WP.3-H(NR)" :: Text) ~> parsePlay
      case res of
        Left err -> fail err
        Right pr -> do
          pr `shouldBe` Play [WildPitch] [] [PlayMovement ThirdBase HomePlate True]
          numRBI pr `shouldBe` 0

    it "should successfully simple annotated double play" $ do
      let
        res = ("54(1)3/GDP" :: Text) ~> parsePlay
      case res of
        Left err -> fail err
        Right pr -> do
          pr `shouldBe` Play [RoutinePlay [ThirdBaseman,SecondBaseman] (Just FirstBase), RoutinePlay [FirstBaseman] Nothing] [OtherDescriptor "GDP"] []
          numRBI pr `shouldBe` 0

    it "should successfully parse strikeout with passed ball and batter advance" $ do
      let
        res = ("K+PB.2-3;B-1" :: Text) ~> parsePlay
      case res of
        Left err -> fail err
        Right p -> do
          p `shouldBe` Play [Strikeout Nothing, PassedBall] [] [PlayMovement SecondBase ThirdBase True, PlayMovement HomePlate FirstBase True]
          numRBI p `shouldBe` 0
          isBatterOut p `shouldBe` False

    it "should successfully parse double with unearned runs and failed batter advancement" $ do
      let
        res = ("D8/F+.3-H(UR);1-H(UR);BX3(843562/TH)" :: Text) ~> parsePlay
      case res of
        Left err -> fail err
        Right p -> do
          p `shouldBe` Play [Hit SecondBase (Just [CenterFielder])] [OtherDescriptor "F+"] [PlayMovement ThirdBase HomePlate True, PlayMovement FirstBase HomePlate True, PlayMovement HomePlate ThirdBase False]
          numRBI p `shouldBe` 2
          isBatterOut p `shouldBe` True

    it "should successfully parse failed stretch on single under review" $ do
      let
        res = ("S8/L/MREV.BX2(84)" :: Text) ~> parsePlay
      case res of
        Left err -> fail err
        Right p -> do
          p `shouldBe` Play [Hit FirstBase (Just [CenterFielder])] [OtherDescriptor "L", OtherDescriptor "MREV"] [PlayMovement HomePlate SecondBase False]
          isBatterOut p `shouldBe` True
          isHit p `shouldBe` True

    it "should successfully parse strikeout with pickoff" $ do
      let
        res = ("K+PO1(23)/DP" :: Text) ~> parsePlay
      case res of
        Left err -> fail err
        Right p -> do
          p `shouldBe` Play [Strikeout Nothing, Pickoff FirstBase (Just [Catcher, FirstBaseman])] [OtherDescriptor "DP"] []
          isBatterOut p `shouldBe` True

  describe "parsePlayAction" $ do

    let
      shouldParseAsPlay :: Text -> PlayAction -> Expectation
      shouldParseAsPlay t pa = do
        t ~> parsePlayAction `shouldParse` pa
        leftover (t ~?> parsePlayAction) `shouldSatisfy` \str -> fromMaybe "" str == ""

    it "should successfully parse HitByPitch" $
      "HP" `shouldParseAsPlay` HitByPitch

    it "should successfully parse Strikeout" $
      "K" `shouldParseAsPlay` Strikeout Nothing

    it "should successfully parse dropped Strikeout" $
      "K23" `shouldParseAsPlay` Strikeout Nothing

    it "should successfully parse basic StolenBase" $
      "SB3" `shouldParseAsPlay` StolenBase ThirdBase

    it "should successfully parse basic CaughtStealing" $
      "CS3(24)" `shouldParseAsPlay` CaughtStealing ThirdBase (Just [Catcher, SecondBaseman])

    it "should successfully parse basic Single" $
      "S7" `shouldParseAsPlay` Hit FirstBase (Just [LeftFielder])

    it "should successfully parse basic Single with advance held to first" $
      "S56" `shouldParseAsPlay` Hit FirstBase (Just [ThirdBaseman, ShortStop])

    it "should successfully parse basic Double" $
      "D8" `shouldParseAsPlay` Hit SecondBase (Just [CenterFielder])

    it "should successfully parse ground-rule Double" $
      "DGR" `shouldParseAsPlay` Hit SecondBase Nothing

    it "should successfully parse basic Triple" $
      "T9" `shouldParseAsPlay` Hit ThirdBase (Just [RightFielder])

    it "should successfully parse basic HomeRun" $
      "HR" `shouldParseAsPlay` Hit HomePlate Nothing

    it "should successfully parse basic inside-the-park HomeRun" $
      "HR7" `shouldParseAsPlay` Hit HomePlate (Just [LeftFielder])

    it "should successfully parse fielders choice" $
      "FC6" `shouldParseAsPlay` FieldersChoice [ShortStop]

    it "should successfully parse routine out" $
      "53" `shouldParseAsPlay` RoutinePlay [ThirdBaseman, FirstBaseman] Nothing

    it "should successfully parse annotated routine out" $
      "46(1)" `shouldParseAsPlay` RoutinePlay [SecondBaseman, ShortStop] (Just FirstBase)

  describe "parsePlayActions" $ do

    let
      shouldParseAsPlays :: Text -> [PlayAction] -> Expectation
      shouldParseAsPlays t pa = do
        t ~> many parsePlayAction `shouldParse` pa
        leftover (t ~?> many parsePlayAction) `shouldSatisfy` \str -> fromMaybe "" str == ""

    it "should successfully parse routine double play" $
      "46(1)3" `shouldParseAsPlays` [RoutinePlay [SecondBaseman, ShortStop] (Just FirstBase), RoutinePlay [FirstBaseman] Nothing]

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

    it "should successfully parse non-RBI advance" $
      ".3-H(NR)" `shouldParseMovements` [PlayMovement ThirdBase HomePlate True]

    it "should successfully parse out multiple movements" $
      ".3-H;2X3(65)" `shouldParseMovements`
        [ PlayMovement ThirdBase HomePlate True
        , PlayMovement SecondBase ThirdBase False
        ]

    it "should successfully parse out unearned advances on batter out" $
      ".3-H(UR);1-H(UR);BX3(843562/TH)" `shouldParseMovements`
        [ PlayMovement ThirdBase HomePlate True
        , PlayMovement FirstBase HomePlate True
        , PlayMovement HomePlate ThirdBase False
        ]

  describe "box score smoke test" $ do
    let smokeScores = map generateBoxScore <$> gamesFromFilePath "testgames.txt"

    it "should return correct stats" $
      smokeScores >>= (\[bs1, bs2, bs3, bs4, bs5, bs6, bs7, bs8, _] -> do
        (battingStats (teamBoxScoreBatting (boxScoreHomeTeam bs1))) `shouldBe` HashMap.fromList 
          [("kimbc001",BattingLine {battingLinePlayerId = "kimbc001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0}),("bogax001",BattingLine {battingLinePlayerId = "bogax001", battingLineAtBats = 4, battingLineRuns = 1, battingLineHits = 1, battingLineRBI = 1, battingLineWalks = 1, battingLineStrikeouts = 1, battingLineLOB = 0}),("ortid001",BattingLine {battingLinePlayerId = "ortid001", battingLineAtBats = 5, battingLineRuns = 0, battingLineHits = 2, battingLineRBI = 1, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 2}),("barnm001",BattingLine {battingLinePlayerId = "barnm001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0}),("ramih003",BattingLine {battingLinePlayerId = "ramih003", battingLineAtBats = 3, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 1, battingLineWalks = 1, battingLineStrikeouts = 2, battingLineLOB = 1}),("holtb002",BattingLine {battingLinePlayerId = "holtb002", battingLineAtBats = 2, battingLineRuns = 2, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 2, battingLineStrikeouts = 0, battingLineLOB = 1}),("younc004",BattingLine {battingLinePlayerId = "younc004", battingLineAtBats = 2, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 2}),("tazaj001",BattingLine {battingLinePlayerId = "tazaj001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0}),("pricd001",BattingLine {battingLinePlayerId = "pricd001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0}),("shawt001",BattingLine {battingLinePlayerId = "shawt001", battingLineAtBats = 2, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 2}),("bettm001",BattingLine {battingLinePlayerId = "bettm001", battingLineAtBats = 5, battingLineRuns = 2, battingLineHits = 3, battingLineRBI = 2, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 2}),("uehak001",BattingLine {battingLinePlayerId = "uehak001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0}),("pedrd001",BattingLine {battingLinePlayerId = "pedrd001", battingLineAtBats = 5, battingLineRuns = 1, battingLineHits = 2, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 2}),("swihb001",BattingLine {battingLinePlayerId = "swihb001", battingLineAtBats = 4, battingLineRuns = 1, battingLineHits = 2, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0}),("bradj001",BattingLine {battingLinePlayerId = "bradj001", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 1, battingLineRBI = 2, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 1})]
        (battingStats (teamBoxScoreBatting (boxScoreAwayTeam bs1))) `shouldBe` HashMap.fromList 
          [("rickj001",BattingLine {battingLinePlayerId = "rickj001", battingLineAtBats = 4, battingLineRuns = 1, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 1, battingLineStrikeouts = 2, battingLineLOB = 2}),("wietm001",BattingLine {battingLinePlayerId = "wietm001", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 4, battingLineLOB = 1}),("gally001",BattingLine {battingLinePlayerId = "gally001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0}),("britz001",BattingLine {battingLinePlayerId = "britz001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0}),("givem001",BattingLine {battingLinePlayerId = "givem001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0}),("jonea003",BattingLine {battingLinePlayerId = "jonea003", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0}),("reimn001",BattingLine {battingLinePlayerId = "reimn001", battingLineAtBats = 3, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 2, battingLineLOB = 2}),("schoj001",BattingLine {battingLinePlayerId = "schoj001", battingLineAtBats = 3, battingLineRuns = 0, battingLineHits = 1, battingLineRBI = 1, battingLineWalks = 1, battingLineStrikeouts = 1, battingLineLOB = 0}),("mcfat001",BattingLine {battingLinePlayerId = "mcfat001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0}),("trumm001",BattingLine {battingLinePlayerId = "trumm001", battingLineAtBats = 5, battingLineRuns = 1, battingLineHits = 1, battingLineRBI = 3, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 3}),("davic003",BattingLine {battingLinePlayerId = "davic003", battingLineAtBats = 5, battingLineRuns = 2, battingLineHits = 2, battingLineRBI = 5, battingLineWalks = 0, battingLineStrikeouts = 3, battingLineLOB = 3}),("josec002",BattingLine {battingLinePlayerId = "josec002", battingLineAtBats = 3, battingLineRuns = 2, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 1, battingLineStrikeouts = 1, battingLineLOB = 2}),("flahr001",BattingLine {battingLinePlayerId = "flahr001", battingLineAtBats = 1, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0}),("machm001",BattingLine {battingLinePlayerId = "machm001", battingLineAtBats = 3, battingLineRuns = 2, battingLineHits = 2, battingLineRBI = 0, battingLineWalks = 1, battingLineStrikeouts = 0, battingLineLOB = 1}),("hardj003",BattingLine {battingLinePlayerId = "hardj003", battingLineAtBats = 4, battingLineRuns = 1, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 0}),("bracb001",BattingLine {battingLinePlayerId = "bracb001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})]
        )
