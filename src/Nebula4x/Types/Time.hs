module Nebula4x.Types.Time where

type Year = Integer

type Years = Integer

type Month = Int

type Months = Int

type Day = Int

type Days = Int

type Hour = Int

type Hours = Int

type Minute = Int

type Minutes = Int

type Second = Int

type Seconds = Second

data TickTime
  = TickSecond Int
  | TickMinute Int
  | TickHour Int
  | TickDay Int
  deriving (Show, Eq)
