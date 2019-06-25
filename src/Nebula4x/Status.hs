{-# LANGUAGE TemplateHaskell #-}

module Nebula4x.Status where

import           Nebula4x.Time  (defaultStartTimeInt)
import           Nebula4x.Types

startingStatus :: Status
startingStatus =
  Status $
  PulseStatus
    (defaultStartTimeInt, defaultStartTimeInt)
    (defaultStartTimeInt, defaultStartTimeInt)
    (defaultStartTimeInt, defaultStartTimeInt)
    (defaultStartTimeInt, defaultStartTimeInt)
    (defaultStartTimeInt, defaultStartTimeInt)
    (defaultStartTimeInt, defaultStartTimeInt)
    (defaultStartTimeInt, defaultStartTimeInt)
    (defaultStartTimeInt, defaultStartTimeInt)
    (defaultStartTimeInt, defaultStartTimeInt)
