{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import ClassyPrelude
import Test.Hspec.Attoparsec
import ProjectScoresheet.BaseballTypes
import ProjectScoresheet.BoxScore
import ProjectScoresheet.BoxScore.Batting
import ProjectScoresheet.Game (gamesFromFilePath)
import ProjectScoresheet.Play
import ProjectScoresheet.Retrosheet.Parser
import Test.Tasty
import Test.Tasty.Hspec
import qualified Data.HashMap.Strict as HashMap

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
    let smokeScores = map generateBoxScore <$> gamesFromFilePath "testgame.txt"

    it "should return correct stats" $
      smokeScores >>= (\[bs1, bs2, bs3, bs4, bs5, bs6, bs7, bs8] -> do
        battingStats (boxScoreBatting bs1) `shouldBe` HashMap.fromList
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
        battingStats (boxScoreBatting bs2) `shouldBe` HashMap.fromList
          [ ("laynt001",BattingLine {battingLinePlayerId = "laynt001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("bogax001",BattingLine {battingLinePlayerId = "bogax001", battingLineAtBats = 4, battingLineRuns = 1, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 1})
          , ("ortid001",BattingLine {battingLinePlayerId = "ortid001", battingLineAtBats = 4, battingLineRuns = 1, battingLineHits = 2, battingLineRBI = 3, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("rickj001",BattingLine {battingLinePlayerId = "rickj001", battingLineAtBats = 5, battingLineRuns = 0, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 2, battingLineLOB = 2})
          , ("wietm001",BattingLine {battingLinePlayerId = "wietm001", battingLineAtBats = 4, battingLineRuns = 1, battingLineHits = 1, battingLineRBI = 2, battingLineWalks = 1, battingLineStrikeouts = 2, battingLineLOB = 3})
          , ("buchc001",BattingLine {battingLinePlayerId = "buchc001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("jonea003",BattingLine {battingLinePlayerId = "jonea003", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("ramih003",BattingLine {battingLinePlayerId = "ramih003", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 1})
          , ("schoj001",BattingLine {battingLinePlayerId = "schoj001", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 2})
          , ("mcfat001",BattingLine {battingLinePlayerId = "mcfat001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("holtb002",BattingLine {battingLinePlayerId = "holtb002", battingLineAtBats = 2, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 1, battingLineStrikeouts = 0, battingLineLOB = 2})
          , ("wrigm001",BattingLine {battingLinePlayerId = "wrigm001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("trumm001",BattingLine {battingLinePlayerId = "trumm001", battingLineAtBats = 4, battingLineRuns = 3, battingLineHits = 3, battingLineRBI = 2, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 2})
          , ("davic003",BattingLine {battingLinePlayerId = "davic003", battingLineAtBats = 3, battingLineRuns = 1, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 2, battingLineStrikeouts = 2, battingLineLOB = 1})
          , ("younc004",BattingLine {battingLinePlayerId = "younc004", battingLineAtBats = 1, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 1})
          , ("rossr002",BattingLine {battingLinePlayerId = "rossr002", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("bundd001",BattingLine {battingLinePlayerId = "bundd001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("shawt001",BattingLine {battingLinePlayerId = "shawt001", battingLineAtBats = 2, battingLineRuns = 0, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("flahr001",BattingLine {battingLinePlayerId = "flahr001", battingLineAtBats = 3, battingLineRuns = 0, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 1, battingLineStrikeouts = 2, battingLineLOB = 1})
          , ("alvap001",BattingLine {battingLinePlayerId = "alvap001", battingLineAtBats = 4, battingLineRuns = 1, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 1, battingLineStrikeouts = 1, battingLineLOB = 2})
          , ("ramin002",BattingLine {battingLinePlayerId = "ramin002", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("machm001",BattingLine {battingLinePlayerId = "machm001", battingLineAtBats = 5, battingLineRuns = 1, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 1})
          , ("bettm001",BattingLine {battingLinePlayerId = "bettm001", battingLineAtBats = 5, battingLineRuns = 1, battingLineHits = 2, battingLineRBI = 1, battingLineWalks = 0, battingLineStrikeouts = 2, battingLineLOB = 0})
          , ("odayd001",BattingLine {battingLinePlayerId = "odayd001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("pedrd001",BattingLine {battingLinePlayerId = "pedrd001", battingLineAtBats = 5, battingLineRuns = 1, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 2})
          , ("swihb001",BattingLine {battingLinePlayerId = "swihb001", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 5})
          , ("hardj003",BattingLine {battingLinePlayerId = "hardj003", battingLineAtBats = 4, battingLineRuns = 2, battingLineHits = 2, battingLineRBI = 5, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 1})
          , ("bradj001",BattingLine {battingLinePlayerId = "bradj001", battingLineAtBats = 4, battingLineRuns = 1, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 1})
          ]
        battingStats (boxScoreBatting bs3) `shouldBe` HashMap.fromList
          [ ("kimbc001",BattingLine {battingLinePlayerId = "kimbc001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("laynt001",BattingLine {battingLinePlayerId = "laynt001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("bogax001",BattingLine {battingLinePlayerId = "bogax001", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 2, battingLineRBI = 2, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 2})
          , ("hanir001",BattingLine {battingLinePlayerId = "hanir001", battingLineAtBats = 3, battingLineRuns = 0, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 2})
          , ("kellj001",BattingLine {battingLinePlayerId = "kellj001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("ortid001",BattingLine {battingLinePlayerId = "ortid001", battingLineAtBats = 2, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 2, battingLineStrikeouts = 0, battingLineLOB = 1})
          , ("rickj001",BattingLine {battingLinePlayerId = "rickj001", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 1, battingLineStrikeouts = 1, battingLineLOB = 4})
          , ("kim-h001",BattingLine {battingLinePlayerId = "kim-h001", battingLineAtBats = 2, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 2, battingLineStrikeouts = 1, battingLineLOB = 1})
          , ("barnm001",BattingLine {battingLinePlayerId = "barnm001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("ramih003",BattingLine {battingLinePlayerId = "ramih003", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 2, battingLineLOB = 4})
          , ("schoj001",BattingLine {battingLinePlayerId = "schoj001", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 2, battingLineLOB = 2})
          , ("holtb002",BattingLine {battingLinePlayerId = "holtb002", battingLineAtBats = 3, battingLineRuns = 1, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 1, battingLineStrikeouts = 1, battingLineLOB = 1})
          , ("jimeu001",BattingLine {battingLinePlayerId = "jimeu001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("trumm001",BattingLine {battingLinePlayerId = "trumm001", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 1})
          , ("davic003",BattingLine {battingLinePlayerId = "davic003", battingLineAtBats = 5, battingLineRuns = 1, battingLineHits = 1, battingLineRBI = 2, battingLineWalks = 0, battingLineStrikeouts = 2, battingLineLOB = 3})
          , ("josec002",BattingLine {battingLinePlayerId = "josec002", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 2, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 2, battingLineLOB = 2})
          , ("tazaj001",BattingLine {battingLinePlayerId = "tazaj001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("shawt001",BattingLine {battingLinePlayerId = "shawt001", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 1})
          , ("alvap001",BattingLine {battingLinePlayerId = "alvap001", battingLineAtBats = 2, battingLineRuns = 0, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 2, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("machm001",BattingLine {battingLinePlayerId = "machm001", battingLineAtBats = 5, battingLineRuns = 1, battingLineHits = 3, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 1})
          , ("bettm001",BattingLine {battingLinePlayerId = "bettm001", battingLineAtBats = 4, battingLineRuns = 1, battingLineHits = 2, battingLineRBI = 1, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("uehak001",BattingLine {battingLinePlayerId = "uehak001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("pedrd001",BattingLine {battingLinePlayerId = "pedrd001", battingLineAtBats = 3, battingLineRuns = 1, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 1, battingLineStrikeouts = 2, battingLineLOB = 0})
          , ("hardj003",BattingLine {battingLinePlayerId = "hardj003", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("bradj001",BattingLine {battingLinePlayerId = "bradj001", battingLineAtBats = 3, battingLineRuns = 1, battingLineHits = 1, battingLineRBI = 1, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 2})
          , ("wilst004",BattingLine {battingLinePlayerId = "wilst004", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          ]
        battingStats (boxScoreBatting bs4) `shouldBe` HashMap.fromList
          [ ("tholj001",BattingLine {battingLinePlayerId = "tholj001", battingLineAtBats = 2, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 0})
          , ("goinr001",BattingLine {battingLinePlayerId = "goinr001", battingLineAtBats = 3, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 0})
          , ("tulot001",BattingLine {battingLinePlayerId = "tulot001", battingLineAtBats = 2, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 1, battingLineStrikeouts = 1, battingLineLOB = 0})
          , ("kimbc001",BattingLine {battingLinePlayerId = "kimbc001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("chavj001",BattingLine {battingLinePlayerId = "chavj001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("bogax001",BattingLine {battingLinePlayerId = "bogax001", battingLineAtBats = 4, battingLineRuns = 1, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 3})
          , ("porcr001",BattingLine {battingLinePlayerId = "porcr001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("ortid001",BattingLine {battingLinePlayerId = "ortid001", battingLineAtBats = 2, battingLineRuns = 1, battingLineHits = 1, battingLineRBI = 1, battingLineWalks = 2, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("saunm001",BattingLine {battingLinePlayerId = "saunm001", battingLineAtBats = 3, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 1})
          , ("pillk001",BattingLine {battingLinePlayerId = "pillk001", battingLineAtBats = 3, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 0})
          , ("smoaj001",BattingLine {battingLinePlayerId = "smoaj001", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 4, battingLineLOB = 3})
          , ("dickr001",BattingLine {battingLinePlayerId = "dickr001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("biagj001",BattingLine {battingLinePlayerId = "biagj001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("ramih003",BattingLine {battingLinePlayerId = "ramih003", battingLineAtBats = 4, battingLineRuns = 1, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 3, battingLineLOB = 2})
          , ("martr004",BattingLine {battingLinePlayerId = "martr004", battingLineAtBats = 1, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 0})
          , ("donaj001",BattingLine {battingLinePlayerId = "donaj001", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 1})
          , ("holtb002",BattingLine {battingLinePlayerId = "holtb002", battingLineAtBats = 3, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 1, battingLineStrikeouts = 0, battingLineLOB = 3})
          , ("bautj002",BattingLine {battingLinePlayerId = "bautj002", battingLineAtBats = 3, battingLineRuns = 1, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 1, battingLineStrikeouts = 1, battingLineLOB = 0})
          , ("encae001",BattingLine {battingLinePlayerId = "encae001", battingLineAtBats = 4, battingLineRuns = 2, battingLineHits = 3, battingLineRBI = 3, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("tazaj001",BattingLine {battingLinePlayerId = "tazaj001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("shawt001",BattingLine {battingLinePlayerId = "shawt001", battingLineAtBats = 2, battingLineRuns = 0, battingLineHits = 1, battingLineRBI = 2, battingLineWalks = 2, battingLineStrikeouts = 1, battingLineLOB = 1})
          , ("vendp001",BattingLine {battingLinePlayerId = "vendp001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("bettm001",BattingLine {battingLinePlayerId = "bettm001", battingLineAtBats = 5, battingLineRuns = 0, battingLineHits = 1, battingLineRBI = 1, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 3})
          , ("uehak001",BattingLine {battingLinePlayerId = "uehak001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("vazqc001",BattingLine {battingLinePlayerId = "vazqc001", battingLineAtBats = 4, battingLineRuns = 2, battingLineHits = 2, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 2})
          , ("pedrd001",BattingLine {battingLinePlayerId = "pedrd001", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 1, battingLineRBI = 1, battingLineWalks = 1, battingLineStrikeouts = 1, battingLineLOB = 2})
          , ("bradj001",BattingLine {battingLinePlayerId = "bradj001", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 2, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 1})
          ]
        battingStats (boxScoreBatting bs5) `shouldBe` HashMap.fromList
          [ ("estrm001",BattingLine {battingLinePlayerId = "estrm001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("tulot001",BattingLine {battingLinePlayerId = "tulot001", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 2})
          , ("kimbc001",BattingLine {battingLinePlayerId = "kimbc001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("barnd001",BattingLine {battingLinePlayerId = "barnd001", battingLineAtBats = 3, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 2, battingLineLOB = 1})
          , ("bogax001",BattingLine {battingLinePlayerId = "bogax001", battingLineAtBats = 4, battingLineRuns = 1, battingLineHits = 1, battingLineRBI = 3, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 1})
          , ("floyg001",BattingLine {battingLinePlayerId = "floyg001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("ortid001",BattingLine {battingLinePlayerId = "ortid001", battingLineAtBats = 3, battingLineRuns = 0, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 1, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("pillk001",BattingLine {battingLinePlayerId = "pillk001", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 2, battingLineLOB = 2})
          , ("ramih003",BattingLine {battingLinePlayerId = "ramih003", battingLineAtBats = 4, battingLineRuns = 1, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 2})
          , ("martr004",BattingLine {battingLinePlayerId = "martr004", battingLineAtBats = 3, battingLineRuns = 0, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 0})
          , ("donaj001",BattingLine {battingLinePlayerId = "donaj001", battingLineAtBats = 4, battingLineRuns = 1, battingLineHits = 2, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 0})
          , ("cecib001",BattingLine {battingLinePlayerId = "cecib001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("holtb002",BattingLine {battingLinePlayerId = "holtb002", battingLineAtBats = 3, battingLineRuns = 0, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 3})
          , ("bautj002",BattingLine {battingLinePlayerId = "bautj002", battingLineAtBats = 4, battingLineRuns = 1, battingLineHits = 2, battingLineRBI = 1, battingLineWalks = 0, battingLineStrikeouts = 2, battingLineLOB = 1})
          , ("encae001",BattingLine {battingLinePlayerId = "encae001", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 1, battingLineRBI = 1, battingLineWalks = 0, battingLineStrikeouts = 2, battingLineLOB = 1})
          , ("pricd001",BattingLine {battingLinePlayerId = "pricd001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("shawt001",BattingLine {battingLinePlayerId = "shawt001", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 2, battingLineRBI = 1, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("colac001",BattingLine {battingLinePlayerId = "colac001", battingLineAtBats = 3, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 1})
          , ("bettm001",BattingLine {battingLinePlayerId = "bettm001", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 2})
          , ("uehak001",BattingLine {battingLinePlayerId = "uehak001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("vazqc001",BattingLine {battingLinePlayerId = "vazqc001", battingLineAtBats = 3, battingLineRuns = 0, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 3})
          , ("pedrd001",BattingLine {battingLinePlayerId = "pedrd001", battingLineAtBats = 3, battingLineRuns = 1, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 1, battingLineStrikeouts = 0, battingLineLOB = 1})
          , ("carre001",BattingLine {battingLinePlayerId = "carre001", battingLineAtBats = 3, battingLineRuns = 0, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 1})
          , ("bradj001",BattingLine {battingLinePlayerId = "bradj001", battingLineAtBats = 3, battingLineRuns = 1, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 1})
          ]
        battingStats (boxScoreBatting bs6) `shouldBe` HashMap.fromList
          [ ("goinr001",BattingLine {battingLinePlayerId = "goinr001", battingLineAtBats = 4, battingLineRuns = 1, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 5})
          , ("tulot001",BattingLine {battingLinePlayerId = "tulot001", battingLineAtBats = 5, battingLineRuns = 0, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 3, battingLineLOB = 3})
          , ("laynt001",BattingLine {battingLinePlayerId = "laynt001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("hernm003",BattingLine {battingLinePlayerId = "hernm003", battingLineAtBats = 2, battingLineRuns = 1, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 1, battingLineStrikeouts = 0, battingLineLOB = 1})
          , ("bogax001",BattingLine {battingLinePlayerId = "bogax001", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 2})
          , ("hanir001",BattingLine {battingLinePlayerId = "hanir001", battingLineAtBats = 3, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 1, battingLineStrikeouts = 1, battingLineLOB = 0})
          , ("ortid001",BattingLine {battingLinePlayerId = "ortid001", battingLineAtBats = 2, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 2, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("saunm001",BattingLine {battingLinePlayerId = "saunm001", battingLineAtBats = 5, battingLineRuns = 1, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 4})
          , ("pillk001",BattingLine {battingLinePlayerId = "pillk001", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 3, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 1})
          , ("barnm001",BattingLine {battingLinePlayerId = "barnm001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("smoaj001",BattingLine {battingLinePlayerId = "smoaj001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 1, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("wrigs001",BattingLine {battingLinePlayerId = "wrigs001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("ramih003",BattingLine {battingLinePlayerId = "ramih003", battingLineAtBats = 4, battingLineRuns = 1, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 2})
          , ("martr004",BattingLine {battingLinePlayerId = "martr004", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 2, battingLineLOB = 6})
          , ("osunr001",BattingLine {battingLinePlayerId = "osunr001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("donaj001",BattingLine {battingLinePlayerId = "donaj001", battingLineAtBats = 5, battingLineRuns = 1, battingLineHits = 2, battingLineRBI = 1, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 1})
          , ("stord001",BattingLine {battingLinePlayerId = "stord001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("younc004",BattingLine {battingLinePlayerId = "younc004", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 3, battingLineLOB = 1})
          , ("bautj002",BattingLine {battingLinePlayerId = "bautj002", battingLineAtBats = 5, battingLineRuns = 1, battingLineHits = 2, battingLineRBI = 2, battingLineWalks = 0, battingLineStrikeouts = 3, battingLineLOB = 1})
          , ("encae001",BattingLine {battingLinePlayerId = "encae001", battingLineAtBats = 5, battingLineRuns = 1, battingLineHits = 2, battingLineRBI = 1, battingLineWalks = 0, battingLineStrikeouts = 2, battingLineLOB = 2})
          , ("rossr002",BattingLine {battingLinePlayerId = "rossr002", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("shawt001",BattingLine {battingLinePlayerId = "shawt001", battingLineAtBats = 4, battingLineRuns = 1, battingLineHits = 1, battingLineRBI = 2, battingLineWalks = 0, battingLineStrikeouts = 2, battingLineLOB = 1})
          , ("sanca006",BattingLine {battingLinePlayerId = "sanca006", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("ramin002",BattingLine {battingLinePlayerId = "ramin002", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("colac001",BattingLine {battingLinePlayerId = "colac001", battingLineAtBats = 3, battingLineRuns = 0, battingLineHits = 1, battingLineRBI = 1, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("bettm001",BattingLine {battingLinePlayerId = "bettm001", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 1, battingLineRBI = 1, battingLineWalks = 0, battingLineStrikeouts = 2, battingLineLOB = 1})
          , ("bradj001",BattingLine {battingLinePlayerId = "bradj001", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 0})]
        battingStats (boxScoreBatting bs7) `shouldBe` HashMap.fromList
          [ ("goinr001",BattingLine {battingLinePlayerId = "goinr001", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 3})
          , ("tulot001",BattingLine {battingLinePlayerId = "tulot001", battingLineAtBats = 3, battingLineRuns = 0, battingLineHits = 1, battingLineRBI = 1, battingLineWalks = 1, battingLineStrikeouts = 0, battingLineLOB = 1})
          , ("kimbc001",BattingLine {battingLinePlayerId = "kimbc001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("barnd001",BattingLine {battingLinePlayerId = "barnd001", battingLineAtBats = 0, battingLineRuns = 1, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("bogax001",BattingLine {battingLinePlayerId = "bogax001", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 1})
          , ("ortid001",BattingLine {battingLinePlayerId = "ortid001", battingLineAtBats = 1, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 1})
          , ("saunm001",BattingLine {battingLinePlayerId = "saunm001", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 1, battingLineRBI = 1, battingLineWalks = 1, battingLineStrikeouts = 0, battingLineLOB = 2})
          , ("pillk001",BattingLine {battingLinePlayerId = "pillk001", battingLineAtBats = 3, battingLineRuns = 1, battingLineHits = 3, battingLineRBI = 0, battingLineWalks = 1, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("smoaj001",BattingLine {battingLinePlayerId = "smoaj001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 1, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("buchc001",BattingLine {battingLinePlayerId = "buchc001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("ramih003",BattingLine {battingLinePlayerId = "ramih003", battingLineAtBats = 4, battingLineRuns = 1, battingLineHits = 2, battingLineRBI = 1, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 1})
          , ("martr004",BattingLine {battingLinePlayerId = "martr004", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 2, battingLineRBI = 2, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 1})
          , ("rutlj001",BattingLine {battingLinePlayerId = "rutlj001", battingLineAtBats = 3, battingLineRuns = 0, battingLineHits = 1, battingLineRBI = 1, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 1})
          , ("donaj001",BattingLine {battingLinePlayerId = "donaj001", battingLineAtBats = 2, battingLineRuns = 1, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 2, battingLineStrikeouts = 1, battingLineLOB = 2})
          , ("cecib001",BattingLine {battingLinePlayerId = "cecib001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("stord001",BattingLine {battingLinePlayerId = "stord001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("happj001",BattingLine {battingLinePlayerId = "happj001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("younc004",BattingLine {battingLinePlayerId = "younc004", battingLineAtBats = 3, battingLineRuns = 0, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 1})
          , ("bautj002",BattingLine {battingLinePlayerId = "bautj002", battingLineAtBats = 3, battingLineRuns = 1, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 1, battingLineStrikeouts = 0, battingLineLOB = 3})
          , ("encae001",BattingLine {battingLinePlayerId = "encae001", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 2, battingLineLOB = 3})
          , ("tazaj001",BattingLine {battingLinePlayerId = "tazaj001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("shawt001",BattingLine {battingLinePlayerId = "shawt001", battingLineAtBats = 4, battingLineRuns = 1, battingLineHits = 2, battingLineRBI = 1, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("ramin002",BattingLine {battingLinePlayerId = "ramin002", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("colac001",BattingLine {battingLinePlayerId = "colac001", battingLineAtBats = 1, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 1})
          , ("bettm001",BattingLine {battingLinePlayerId = "bettm001", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 1})
          , ("uehak001",BattingLine {battingLinePlayerId = "uehak001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("vazqc001",BattingLine {battingLinePlayerId = "vazqc001", battingLineAtBats = 3, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 2})
          , ("pedrd001",BattingLine {battingLinePlayerId = "pedrd001", battingLineAtBats = 4, battingLineRuns = 1, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 1})
          , ("carre001",BattingLine {battingLinePlayerId = "carre001", battingLineAtBats = 2, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 2})
          , ("bradj001",BattingLine {battingLinePlayerId = "bradj001", battingLineAtBats = 2, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 1, battingLineStrikeouts = 0, battingLineLOB = 1})
          ]
        battingStats (boxScoreBatting bs8) `shouldBe` HashMap.fromList
          [ ("millb002",BattingLine {battingLinePlayerId = "millb002", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 1, battingLineStrikeouts = 2, battingLineLOB = 1})
          , ("kimbc001",BattingLine {battingLinePlayerId = "kimbc001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("kierk001",BattingLine {battingLinePlayerId = "kierk001", battingLineAtBats = 4, battingLineRuns = 1, battingLineHits = 1, battingLineRBI = 1, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 1})
          , ("morrl001",BattingLine {battingLinePlayerId = "morrl001", battingLineAtBats = 5, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 4})
          , ("laynt001",BattingLine {battingLinePlayerId = "laynt001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("bogax001",BattingLine {battingLinePlayerId = "bogax001", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 2, battingLineLOB = 0})
          , ("casac001",BattingLine {battingLinePlayerId = "casac001", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 4, battingLineLOB = 1})
          , ("hanir001",BattingLine {battingLinePlayerId = "hanir001", battingLineAtBats = 2, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 1, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("kellj001",BattingLine {battingLinePlayerId = "kellj001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("ortid001",BattingLine {battingLinePlayerId = "ortid001", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 3, battingLineLOB = 0})
          , ("guyeb001",BattingLine {battingLinePlayerId = "guyeb001", battingLineAtBats = 1, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 2})
          , ("barnm001",BattingLine {battingLinePlayerId = "barnm001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("smyld001",BattingLine {battingLinePlayerId = "smyld001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("dickc002",BattingLine {battingLinePlayerId = "dickc002", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 2, battingLineLOB = 3})
          , ("ramih003",BattingLine {battingLinePlayerId = "ramih003", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 2, battingLineLOB = 0})
          , ("coloa001",BattingLine {battingLinePlayerId = "coloa001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("younc004",BattingLine {battingLinePlayerId = "younc004", battingLineAtBats = 2, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 1, battingLineStrikeouts = 1, battingLineLOB = 0})
          , ("forsl001",BattingLine {battingLinePlayerId = "forsl001", battingLineAtBats = 3, battingLineRuns = 1, battingLineHits = 2, battingLineRBI = 0, battingLineWalks = 2, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("tazaj001",BattingLine {battingLinePlayerId = "tazaj001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("rossr002",BattingLine {battingLinePlayerId = "rossr002", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("shawt001",BattingLine {battingLinePlayerId = "shawt001", battingLineAtBats = 3, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 2, battingLineLOB = 0})
          , ("hembh001",BattingLine {battingLinePlayerId = "hembh001", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("bettm001",BattingLine {battingLinePlayerId = "bettm001", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 2, battingLineLOB = 3})
          , ("pedrd001",BattingLine {battingLinePlayerId = "pedrd001", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 2, battingLineLOB = 3})
          , ("ramie004",BattingLine {battingLinePlayerId = "ramie004", battingLineAtBats = 0, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("bradj001",BattingLine {battingLinePlayerId = "bradj001", battingLineAtBats = 3, battingLineRuns = 0, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 0, battingLineLOB = 0})
          , ("longe001",BattingLine {battingLinePlayerId = "longe001", battingLineAtBats = 3, battingLineRuns = 1, battingLineHits = 1, battingLineRBI = 0, battingLineWalks = 2, battingLineStrikeouts = 1, battingLineLOB = 1})
          , ("souzs001",BattingLine {battingLinePlayerId = "souzs001", battingLineAtBats = 5, battingLineRuns = 0, battingLineHits = 0, battingLineRBI = 0, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 5})
          , ("jennd002",BattingLine {battingLinePlayerId = "jennd002", battingLineAtBats = 4, battingLineRuns = 0, battingLineHits = 1, battingLineRBI = 2, battingLineWalks = 0, battingLineStrikeouts = 1, battingLineLOB = 2})
          ]
        )
