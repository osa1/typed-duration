{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}

-- | Definition of `Duration` and various other types, constructors and functions for building
-- `Duration`s.
module Data.Duration
  (
    -- * Duration type
    Duration (..)
  , DurationUnit (..)
  , (#)
  , pattern Microseconds
  , pattern Milliseconds
  , pattern Seconds
  , pattern Minutes
  , pattern Hours
  , pattern Days
  , pattern Weeks

    -- * Non-operators for building durations
  , microseconds
  , milliseconds
  , seconds
  , minutes
  , hours
  , days
  , weeks

    -- * `Num` types for building durations from integer literals
  , Microseconds (..)
  , Microsecond
  , Milliseconds (..)
  , Millisecond
  , Seconds (..)
  , Second
  , Minutes (..)
  , Minute
  , Hours (..)
  , Hour
  , Days (..)
  , Day
  , Weeks (..)
  , Week
  , ToDuration (..)
  ) where

--------------------------------------------------------------------------------

-- | A microsecond precision duration, represented as an integer.
newtype Duration = Duration
  { -- | Duration in microseconds
    durationUs :: Int
  } deriving (Eq, Ord, Num)

instance Show Duration where
  show (Duration us) = show (Microseconds_ us)

-- | `Duration` units to be used with `#`. See constructor docs for max duration
-- values for each unit.
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

--------------------------------------------------------------------------------
-- * `Num` types and `ToDuration` typeclass for building durations with just
-- integer literals and type annotations.

newtype Microseconds = Microseconds_ { microsecondsInt :: Int }
  deriving (Eq, Ord, Num)

type Microsecond = Microseconds

instance Show Microseconds where
  show (Microseconds_ t) = show t ++ " μs"

newtype Milliseconds = Milliseconds_ { millisecondsInt :: Int }
  deriving (Eq, Ord, Num)

type Millisecond = Milliseconds

instance Show Milliseconds where
  show (Milliseconds_ t) = show t ++ " ms"

newtype Seconds = Seconds_ { secondsInt :: Int }
  deriving (Eq, Ord, Num)

type Second = Seconds

instance Show Seconds where
  show (Seconds_ t) = show t ++ " sec"

newtype Minutes = Minutes_ { minutesInt :: Int }
  deriving (Eq, Ord, Num)

type Minute = Minutes

instance Show Minutes where
  show (Minutes_ t) = show t ++ " min"

newtype Hours = Hours_ { hoursInt :: Int }
  deriving (Eq, Ord, Num)

type Hour = Hours

instance Show Hours where
  show (Hours_ t) = show t ++ " hours"

newtype Days = Days_ { daysInt :: Int }
  deriving (Eq, Ord, Num)

type Day = Days

instance Show Days where
  show (Days_ t) = show t ++ " days"

newtype Weeks = Weeks_ { weeksInt :: Int }
  deriving (Eq, Ord, Num)

type Week = Weeks

microseconds, milliseconds, seconds, minutes, hours, days, weeks :: Int -> Duration
microseconds t = t # Microseconds
milliseconds t = t # Milliseconds
seconds t = t # Seconds
minutes t = t # Minutes
hours t = t # Hours
days t = t # Days
weeks t = t # Weeks

class ToDuration a where
  toDuration :: a -> Duration

instance ToDuration Duration where
  toDuration = id

instance ToDuration Microseconds where
  toDuration (Microseconds_ t) = t # Microseconds

instance ToDuration Milliseconds where
  toDuration (Milliseconds_ t) = t # Milliseconds

instance ToDuration Seconds where
  toDuration (Seconds_ t) = t # Seconds

instance ToDuration Minutes where
  toDuration (Minutes_ t) = t # Minutes

instance ToDuration Hours where
  toDuration (Hours_ t) = t # Hours

instance ToDuration Days where
  toDuration (Days_ t) = t # Days

instance ToDuration Weeks where
  toDuration (Weeks_ t) = t # Weeks
