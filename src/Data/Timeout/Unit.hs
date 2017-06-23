{-# LANGUAGE PatternSynonyms #-}

module Data.Timeout.Unit
  ( Timeout (..)
  , TimeoutUnit (..)
  , pattern Microseconds
  , pattern Milliseconds
  , pattern Seconds
  , pattern Minutes
  , pattern Hours
  , pattern Days
  , pattern Weeks
  , microseconds
  , milliseconds
  , seconds
  , minutes
  , hours
  , days
  , weeks
  , (#)
  ) where

newtype Timeout
  = Timeout { timeoutUs :: Int }
  deriving (Show, Eq, Ord)

-- | See constructor docs for max timeout values for each unit.
data TimeoutUnit
  -- | Max bound: 9223372036854775807
  = Microsecond
  -- | Max bound: 9223372036854775
  | Millisecond
  -- | Max bound: 9223372036854
  | Second
  -- | Max bound: 153722867280
  | Minute
  -- | Max bound: 2562047788
  | Hour
  -- | Max bound: 106751991
  | Day
  -- | Max bound: 15250284
  | Week
  deriving (Show, Eq)

pattern Microseconds :: TimeoutUnit
pattern Microseconds = Microsecond

pattern Milliseconds :: TimeoutUnit
pattern Milliseconds = Millisecond

pattern Seconds :: TimeoutUnit
pattern Seconds = Second

pattern Minutes :: TimeoutUnit
pattern Minutes = Minute

pattern Hours :: TimeoutUnit
pattern Hours = Hour

pattern Days :: TimeoutUnit
pattern Days = Day

pattern Weeks :: TimeoutUnit
pattern Weeks = Week

-- | Make a timeout from a value + unit. Note that this function does not check
-- overflows. See `TimeoutUnit` for max bounds for units.
(#) :: Int -> TimeoutUnit -> Timeout
t # u =
    Timeout $
    case u of
      Microsecond -> t
      Millisecond -> t * 1000
      Second      -> t * 1000000
      Minute      -> t * 60 * 1000000
      Hour        -> t * 60 * 60 * 1000000
      Day         -> t * 24 * 60 * 60 * 1000000
      Week        -> t * 7 * 24 * 60 * 60 * 1000000

microseconds, milliseconds, seconds, minutes, hours, days, weeks :: Int -> Timeout
microseconds t = t # Microseconds
milliseconds t = t # Milliseconds
seconds t = t # Seconds
minutes t = t # Minutes
hours t = t # Hours
days t = t # Days
weeks t = t # Weeks
