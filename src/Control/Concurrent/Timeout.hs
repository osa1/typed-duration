{-# LANGUAGE CPP #-}

module Control.Concurrent.Timeout
  ( threadDelay
  , timeout
  , module Data.Timeout.Unit
  ) where

--------------------------------------------------------------------------------
import qualified Control.Concurrent as C
import           Data.Timeout.Unit
import qualified System.Timeout     as T
--------------------------------------------------------------------------------

threadDelay :: Timeout -> IO ()
threadDelay = C.threadDelay . timeoutUs

timeout :: Timeout -> IO a -> IO (Maybe a)
timeout = T.timeout . timeoutUs
