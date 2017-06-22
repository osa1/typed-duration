{-# LANGUAGE FlexibleContexts #-}

module Control.Concurrent.Timeout
  ( threadDelay
  , timeout
  , module Data.Timeout.Unit
  ) where

--------------------------------------------------------------------------------
import qualified Control.Concurrent.Lifted   as C
import           Control.Monad.Base          (MonadBase)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Timeout.Unit
import qualified System.Timeout.Lifted       as T
--------------------------------------------------------------------------------

threadDelay :: MonadBase IO m => Timeout -> m ()
threadDelay = C.threadDelay . timeoutUs

timeout :: MonadBaseControl IO m => Timeout -> m a -> m (Maybe a)
timeout = T.timeout . timeoutUs
