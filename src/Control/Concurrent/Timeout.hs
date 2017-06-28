{-# LANGUAGE FlexibleContexts #-}

module Control.Concurrent.Timeout
  ( threadDelay
  , timeout
  , module Data.Duration.Unit
  ) where

--------------------------------------------------------------------------------
import qualified Control.Concurrent.Lifted   as C
import           Control.Monad.Base          (MonadBase)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Duration.Unit
import qualified System.Timeout.Lifted       as T
--------------------------------------------------------------------------------

threadDelay :: (MonadBase IO m, ToDuration t) => t -> m ()
threadDelay = C.threadDelay . durationUs . toDuration

timeout :: (MonadBaseControl IO m, ToDuration t) => t -> m a -> m (Maybe a)
timeout = T.timeout . durationUs . toDuration
