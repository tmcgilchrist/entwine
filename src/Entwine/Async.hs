{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Entwine.Async (
    AsyncTimeout (..)
  , renderAsyncTimeout
  , waitWithTimeout
  , waitEitherBoth
  ) where


import           Control.Concurrent.Async (Async, waitSTM, waitEither)
import           Control.Concurrent.Async (async, cancel, wait, AsyncCancelled)
import           Control.Concurrent.STM (atomically, orElse, retry)
import           Control.Monad.Catch (catches, throwM, Handler(..))
import           Control.Monad.Trans.Either (EitherT, left)

import           Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Text as T

import           Entwine.P
import           Entwine.Snooze

import           System.IO (IO)

newtype AsyncTimeout =
  AsyncTimeout Duration
  deriving (Eq, Show)

renderAsyncTimeout :: AsyncTimeout -> Text
renderAsyncTimeout e =
  case e of
    AsyncTimeout d ->
      mconcat [
          "Async took greater than '"
        , T.pack . show $ toSeconds d
        , " seconds' to return."
        ]

waitWithTimeout :: Async a -> Duration -> EitherT AsyncTimeout IO a
waitWithTimeout a d = do
  r <- liftIO $ newIORef False
  s <- liftIO . async $ snooze d
  e <- liftIO $ waitEither a s
  case e of
    Left a' ->
      pure a'
    Right _ -> do
      liftIO $ writeIORef r True
      liftIO $ cancel a
      (liftIO $ wait a)
        `catches` [ Handler (\ (ax :: AsyncCancelled) ->
                                liftIO (readIORef r) >>=
                                  bool (liftIO $ throwM ax) (left $ AsyncTimeout d)
                            )]

waitEitherBoth :: Async a -> Async b -> Async c -> IO (Either a (b, c))
waitEitherBoth a b c =
  atomically $ do
    let
      l = waitSTM a
      r = do
        bb <- waitSTM b `orElse` (waitSTM c >> retry)
        cc <- waitSTM c
        return (bb, cc)
    fmap Left l `orElse` fmap Right r
