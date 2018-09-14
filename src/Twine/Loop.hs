{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
module Twine.Loop (
    loop
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)

import           Twine.Data.Gate
import           Twine.Data.Duration
import           Twine.P
import           Twine.Snooze

-- | Loop with a delay until the gate is closed
--
loop :: MonadIO m => Duration -> Gate -> m () -> m ()
loop d c action = do
  action
  liftIO (isOpen c) >>= \case
      False ->
        return ()
      True -> do
        liftIO $ snooze d
        loop d c action
