-- file Spec.hs
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Planner

timetable :: [[String]]

timetable = [["Penzance", "St Erth", "Camborne", "Redruth", "Truro", "St Austell", "Par", "Bodmin Parkway", "Liskeard", "Plymouth", "Exeter St Davids", "Reading", "London Paddington"],
             ["0844", "0854", "0907", "0914", "0927", "0944", "0951", "1003", "1016", "1040", "1137", "1316", "1344"],
             ["1000", "1010", "1023", "1030", "1043", "1100", "1108", "1119", "1133", "1157", "1302", "1450", "1521"],
             ["1047", "1057", "1112", "1119", "1132", "1150", "1157", "1208", "1221", "1252", "1357", "1539", "1602"]]

main :: IO ()
main = hspec $ do
  describe "Planner.duration" $ do
    it "should report the duration of journey between two stations" $ do
      duration timetable "0907" "Camborne" "Exeter St Davids" `shouldBe` (150 :: Int)

    it "should report the duration for first available train" $ do
      duration timetable "1023" "Camborne" "Exeter St Davids" `shouldBe` (159 :: Int)

    it "should report the duration including waiting time at platform" $ do 
      duration timetable "1101" "St Austell" "Par" `shouldBe` (56 :: Int)

    it "should report the fastest train between two stations" $ do 
      fastestTrain timetable "Exeter St Davids" "London Paddington" `shouldBe` ("1357" :: String)