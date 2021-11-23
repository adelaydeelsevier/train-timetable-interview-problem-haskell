module Planner
  where
    import Data.Map

    makeTrains :: [[String]] -> [Map String String]
    makeTrains timetable =
      Prelude.map (makeTrain route) journeys
      where
        route = head timetable
        journeys = tail timetable

    makeStopList :: [String] -> [String] -> [(String, String)]
    makeStopList [] _ = []
    makeStopList _ [] = []
    makeStopList (station:stations) (time:times) =
      (station, time) : makeStopList stations times

    makeTrain :: [String] -> [String] -> Map String String
    makeTrain route journey = Data.Map.fromList $ makeStopList route journey

    duration :: [[String]] -> String -> String -> String -> Int
    duration timetable passengerArrivalTime startStationName endStationName =
      endStationTimeDuration - arrivalTimeDuration
      where 
        endStationTime = minimum [Data.Map.findWithDefault "0" endStationName train | train <- makeTrains timetable, passengerArrivalTime <= Data.Map.findWithDefault "0" startStationName train]
        endStationTimeDuration = read(Prelude.take 2 endStationTime) * 60 + read(Prelude.drop 2 endStationTime)
        arrivalTimeDuration = read(Prelude.take 2 passengerArrivalTime) * 60 + read(Prelude.drop 2 passengerArrivalTime)

    fastestTrain :: [[String]] -> String -> String -> String
    fastestTrain timetable startStationName endStationName =
      sTime
      where 
        trainTimings = [(Data.Map.findWithDefault "0" startStationName train, Data.Map.findWithDefault "0" endStationName train) | train <- makeTrains timetable]
        (_, sTime) = minimum [(
            read(Prelude.take 2 endTime) * 60 + 
            read(Prelude.drop 2 endTime) -
            read(Prelude.take 2 startTime) * 60 - 
            read(Prelude.drop 2 startTime) :: Int,
            startTime
          ) | (startTime, endTime) <- trainTimings]
