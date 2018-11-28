{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
module Entwine.Loop (
    loop
  ) where

import           Entwine.Data.Gate
import           Entwine.Data.Duration
import           Entwine.P
import           Entwine.Snooze

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
