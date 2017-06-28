{-# LANGUAGE PatternSynonyms #-}

module Data.Duration.Unit
  ( Duration (..)
  , DurationUnit (..)
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

newtype Duration
  = Duration { durationUs :: Int }
  deriving (Show, Eq, Ord)

-- | See constructor docs for max duration values for each unit.
data DurationUnit
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

pattern Microseconds :: DurationUnit
pattern Microseconds = Microsecond

pattern Milliseconds :: DurationUnit
pattern Milliseconds = Millisecond

pattern Seconds :: DurationUnit
pattern Seconds = Second

pattern Minutes :: DurationUnit
pattern Minutes = Minute

pattern Hours :: DurationUnit
pattern Hours = Hour

pattern Days :: DurationUnit
pattern Days = Day

pattern Weeks :: DurationUnit
pattern Weeks = Week

-- | Make a duration from a value + unit. Note that this function does not check
-- overflows. See `DurationUnit` for max bounds for units.
(#) :: Int -> DurationUnit -> Duration
t # u =
    Duration $
    case u of
      Microsecond -> t
      Millisecond -> t * 1000
      Second      -> t * 1000000
      Minute      -> t * 60 * 1000000
      Hour        -> t * 60 * 60 * 1000000
      Day         -> t * 24 * 60 * 60 * 1000000
      Week        -> t * 7 * 24 * 60 * 60 * 1000000

microseconds, milliseconds, seconds, minutes, hours, days, weeks :: Int -> Duration
microseconds t = t # Microseconds
milliseconds t = t # Milliseconds
seconds t = t # Seconds
minutes t = t # Minutes
hours t = t # Hours
days t = t # Days
weeks t = t # Weeks
