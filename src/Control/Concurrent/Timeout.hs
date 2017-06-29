{-# LANGUAGE FlexibleContexts #-}

-- | Lifted and typed `threadDelay` and `timeout` functions.
module Control.Concurrent.Timeout
  ( threadDelay
  , timeout
  , module Data.Duration
  ) where

--------------------------------------------------------------------------------
import qualified Control.Concurrent.Lifted   as C
import           Control.Monad.Base          (MonadBase)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Duration
import qualified System.Timeout.Lifted       as T
--------------------------------------------------------------------------------

-- | Examples:
--
-- @
--     threadDelay (1 :: Second)
--     threadDelay (100 :: Microseconds)
--     threadDelay (1 \# Second + 50 \# Milliseconds)
-- @
--
threadDelay :: (MonadBase IO m, ToDuration t) => t -> m ()
threadDelay = C.threadDelay . durationUs . toDuration

timeout :: (MonadBaseControl IO m, ToDuration t) => t -> m a -> m (Maybe a)
timeout = T.timeout . durationUs . toDuration
