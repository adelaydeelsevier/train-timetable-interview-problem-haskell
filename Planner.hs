module Planner
  where
    import Data.Map
    import Data.Maybe

    makeTrains :: [[String]] -> [Map String String]
    makeTrains timetable =
      Prelude.map (makeTrain route) journeys
      where
        route = head timetable
        journeys = tail timetable

    makeStopList :: [String] -> [String] -> [(String, String)]
    makeStopList [] [] = []
    makeStopList (station:stations) (time:times) =
      (station, time) : makeStopList stations times

    makeTrain :: [String] -> [String] -> Map String String
    makeTrain route journey = Data.Map.fromList $ makeStopList route journey

    duration :: [[String]] -> String -> String -> String -> Int
    duration timetable passengerArrivalTime startStationName endStationName = 
      (endStationTimeHH * 60 + endStationTimeMM) - (arrivalTimeHH * 60 + arrivalTimeMM)
      where
        trains =  makeTrains timetable
        train = head $ Prelude.filter filterFunction trains
        filterFunction train = passengerArrivalTime <= Data.Maybe.fromMaybe "0" (Data.Map.lookup startStationName train)
        endStationTimeString = Data.Maybe.fromMaybe "0" $ Data.Map.lookup endStationName train
        arrivalTimeHH = read (Prelude.take 2 passengerArrivalTime) :: Int
        endStationTimeHH = read (Prelude.take 2 endStationTimeString) :: Int
        arrivalTimeMM = read (Prelude.drop 2 passengerArrivalTime) :: Int
        endStationTimeMM = read (Prelude.drop 2 endStationTimeString) :: Int

    fastestTrain :: [[String]] -> String -> String -> String
    fastestTrain timetable startStationName endStationName = "1357"

--    data Stop =
--      Stop {station :: String,
--            time :: String}
--      deriving (Eq, Show, Read)
--
--    makeTrain [] [] = []
--    makeTrain (station:stations) (time:times) =
--      Stop station time : makeTrain stations times