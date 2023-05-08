module LogAnalysisSpec (spec) where

import LogAnalysis
import Provided.Log
import Test.Hspec

import Paths_cis1940_solutions (getDataFileName)

spec :: Spec
spec = do
  describe "testParse" $ do
    it "should parse and return the logs successfully" $ do
      testLog <- getDataFileName "test.log"
      testParse parse 1 testLog
        `shouldReturn` [LogMessage Info 100 "done"]
  describe "testWhatWentWrong" $ do
    it "should parse and return the logs successfully" $ do
      testLog <- getDataFileName "test.log"
      testWhatWentWrong parse whatWentWrong testLog
        `shouldReturn` ["mid error", "high error"]
  describe "parseMessage" $ do
    context "with well-formed log lines" $ do
      it "should parse a valid warning log line" $ do
        parseMessage "W 662 this if fine"
          `shouldBe` LogMessage Warning 662 "this if fine"
      it "should parse a valid error log line" $ do
        parseMessage "E 2 562 its broken"
          `shouldBe` LogMessage (Error 2) 562 "its broken"
      it "should parse a valid info log line" $ do
        parseMessage "I 29 doing something, wait up"
          `shouldBe` LogMessage Info 29 "doing something, wait up"
    context "with ill-formed log lines" $ do
      it "should return unknown for an incompatible log line" $ do
        parseMessage "This is not in the right format"
          `shouldBe` Unknown "This is not in the right format"
      it "should return Unknown for a malformed timestamp error entry" $ do
        parseMessage "E X le le le"
          `shouldBe` Unknown "E X le le le"
      it "should return Unknown for a malformed timestamp warning entry" $ do
        parseMessage "W X le le le"
          `shouldBe` Unknown "W X le le le"
      it "should return Unknown for a malformed timestamp info entry" $ do
        parseMessage "I X le le le"
          `shouldBe` Unknown "I X le le le"
  describe "insert" $ do
    let newTree lm = Node Leaf lm Leaf
        setLeft _ Leaf           = Leaf
        setLeft nl (Node _ lm r) = Node nl lm r
        setRight _ Leaf           = Leaf
        setRight nr (Node l lm _) = Node l lm nr
    context "when a hand-crafted tree with an unknown root is used" $ do
      it "replaces the root with the valid log message provided" $ do
        let lm = LogMessage Info 123 "something was done"
            tree = Node Leaf (Unknown "oh no") Leaf
        insert lm tree `shouldBe` Node Leaf lm Leaf
    context "when operating on any tree" $ do
      it "should skip inserting unknown log entries" $ do
        insert (Unknown "lorem ipsum") Leaf `shouldBe` Leaf
    context "when operating on an empty tree" $ do
      it "should insert as root" $ do
        let new = LogMessage Info 1 "one"
        insert new Leaf `shouldBe` newTree new
    context "when operating on a root-only tree" $ do
      let baseRoot = LogMessage Info 5 "five"
          baseTree = newTree baseRoot
      it "should insert a lower timestamp to the left" $ do
        let testLog = LogMessage Info 1 "one"
            left = newTree testLog
            target = setLeft left $ newTree baseRoot
        insert testLog baseTree `shouldBe` target
      it "should insert a higher timestamp to the right" $ do
        let testLog = LogMessage Info 9 "nine"
            right = newTree testLog
            target = setRight right $ newTree baseRoot
        insert testLog baseTree `shouldBe` target
    context "when operating on a 2-level tree" $ do
      let rootLog = LogMessage Info 5 "five"
          baseRoot = newTree rootLog
          baseLeft = newTree (LogMessage Info 2 "two")
          baseRight = newTree (LogMessage Info 8 "eight")
          baseTree = setLeft baseLeft . setRight baseRight $ baseRoot
      it "should insert a lowest timestamp to the left-left" $ do
        let testLog = LogMessage Info 1 "one"
            newLeft = newTree testLog
            targetSub = setLeft newLeft baseLeft
            target = setLeft targetSub . setRight baseRight $ baseRoot
        insert testLog baseTree `shouldBe` target
      it "should insert a low timestamp to the left-right" $ do
        let testLog = LogMessage Info 3 "three"
            newRight = newTree testLog
            targetSub = setRight newRight baseLeft
            target = setLeft targetSub . setRight baseRight $ baseRoot
        insert testLog baseTree `shouldBe` target
      it "should insert a high timestamp to the left-right" $ do
        let testLog = LogMessage Info 7 "seven"
            newLeft = newTree testLog
            targetSub = setLeft newLeft baseRight
            target = setRight targetSub . setLeft baseLeft $ baseRoot
        insert testLog baseTree `shouldBe` target
      it "should insert a highest timestamp to the right-right" $ do
        let testLog = LogMessage Info 9 "nine"
            newRight = newTree testLog
            targetSub = setRight newRight baseRight
            target = setRight targetSub . setLeft baseLeft $ baseRoot
        insert testLog baseTree `shouldBe` target
      it "should ignore a duplicated timestamp entry" $ do
        insert rootLog baseRoot `shouldBe` baseRoot
  describe "build" $ do
    context "when an empty list is given" $ do
      it "returns a leaf tree" $ do
        build [] `shouldBe` Leaf
    context "when a single log message is given" $ do
      it "returns log as the tree root" $ do
        let testLog = LogMessage Info 1 "one"
        build [testLog] `shouldBe` Node Leaf testLog Leaf
  describe "inOrder" $ do
    context "when an empty tree is given" $ do
      it "returns an empty list" $ do
        inOrder Leaf `shouldBe` []
    context "when a filled tree is given" $ do
      it "returns logs sorted" $ do
        let log1 = LogMessage Info 1 "one"
            log2 = LogMessage Info 2 "two"
            log3 = LogMessage Info 3 "three"
            log4 = LogMessage Info 4 "four"
            log5 = LogMessage Info 5 "five"
            log6 = LogMessage Info 6 "six"
            log7 = LogMessage Info 7 "seven"
            source = [log7, log3, log2, log5, log6, log4, log1]
            target = [log1, log2, log3, log4, log5, log6, log7]
        inOrder (build source) `shouldBe` target
  describe "whatWentWrong" $ do
    context "when an empty log is given" $ do
      it "returns an empty list" $ do
        whatWentWrong [] `shouldBe` []
        whatWentWrong' [] `shouldBe` []
    context "when provided with arbitrary logs" $ do
      it "returns only errors with severity >= 50" $ do
        let logData =
              unlines
                [ "I 6 Completed armadillo processing",
                  "I 1 Nothing to report",
                  "E 99 10 Flange failed!",
                  "I 4 Everything normal",
                  "I 11 Initiating self-destruct sequence",
                  "E 70 3 Way too many pickles",
                  "E 65 8 Bad pickle-flange interaction detected",
                  "W 5 Flange is due for a check-up",
                  "I 7 Out for lunch, back in two time steps",
                  "E 20 2 Too many pickles",
                  "I 9 Back from lunch"
                ]
            logs = inOrder . build $ parse logData
            target =
              [ "Way too many pickles",
                "Bad pickle-flange interaction detected",
                "Flange failed!"
              ]
        whatWentWrong logs `shouldBe` target
        whatWentWrong' logs `shouldBe` target
  describe "message" $ do
    context "an ill-formed log entry" $ do
      let entry = "foo bar baz qux"
      it "returns the entire log line" $ do
        (message $ parseMessage entry) `shouldBe` entry
