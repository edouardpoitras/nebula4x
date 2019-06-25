module Nebula4x.Time where

import           Data.Char             (isDigit, toLower)
import           Data.Time.Calendar    (fromGregorian)
import           Data.Time.Clock       (NominalDiffTime, UTCTime (UTCTime),
                                        addUTCTime, secondsToDiffTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import           Nebula4x.Types

numSeconds :: TickTime -> Seconds
numSeconds (TickSecond t) = t
numSeconds (TickMinute t) = t * minuteInSeconds
numSeconds (TickHour t)   = t * hourInSeconds
numSeconds (TickDay t)    = t * dayInSeconds

timeToInt :: UTCTime -> Int
timeToInt = round . utcTimeToPOSIXSeconds

minuteInSeconds :: Seconds
minuteInSeconds = 60

hourInSeconds :: Seconds
hourInSeconds = 3600

dayInSeconds :: Seconds
dayInSeconds = 86400

yearInSeconds :: Seconds
yearInSeconds = 31557600

addSecond :: UTCTime -> UTCTime
addSecond = addSeconds 1

addSeconds :: Seconds -> UTCTime -> UTCTime
addSeconds = addUTCTime . fromIntegral

addMinute :: UTCTime -> UTCTime
addMinute = addMinutes 1

addMinutes :: Minutes -> UTCTime -> UTCTime
addMinutes t =
  addUTCTime (fromIntegral (minuteInSeconds * t) :: NominalDiffTime)

addHour :: UTCTime -> UTCTime
addHour = addHours 1

addHours :: Hours -> UTCTime -> UTCTime
addHours t = addUTCTime (fromIntegral (hourInSeconds * t) :: NominalDiffTime)

addDay :: UTCTime -> UTCTime
addDay = addDays 1

addDays :: Days -> UTCTime -> UTCTime
addDays t = addUTCTime (fromIntegral (dayInSeconds * t) :: NominalDiffTime)

tickTimeFromString :: String -> TickTime
tickTimeFromString str = tickTime
  where
    digits = takeWhile isDigit str
    granularity = map toLower $ takeWhile (not . isDigit) (reverse str)
    number =
      if length digits < 1
        then 1
        else read digits :: Int
    tickTime =
      case granularity of
        "s"       -> TickSecond number
        "sec"     -> TickSecond number
        "second"  -> TickSecond number
        "seconds" -> TickSecond number
        "m"       -> TickMinute number
        "min"     -> TickMinute number
        "minute"  -> TickMinute number
        "minutes" -> TickMinute number
        "h"       -> TickHour number
        "hr"      -> TickHour number
        "hour"    -> TickHour number
        "hours"   -> TickHour number
        "d"       -> TickDay number
        "day"     -> TickDay number
        "days"    -> TickDay number
        _         -> TickSecond number

--
-- Helper function to create a new UTCTime
--
newUTCTime :: Year -> Month -> Day -> Hour -> Minute -> Second -> UTCTime
newUTCTime y m d hr mn sec =
  UTCTime (fromGregorian y m d) (secondsToDiffTime totalSeconds)
  where
    totalSeconds = fromIntegral $ sec + minSeconds + hrSeconds
    minSeconds = mn * minuteInSeconds
    hrSeconds = hr * hourInSeconds

--
-- The default start time for a new universe
--
defaultStartTime :: UTCTime
defaultStartTime = newUTCTime 2025 1 1 0 0 0

defaultStartTimeInt :: Int
defaultStartTimeInt = timeToInt defaultStartTime
