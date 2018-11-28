{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
module Entwine.Parallel (
  -- * Types
    RunError (..)
  -- * Functions
  , renderRunError
  , consume
  , consume_
  , waitEitherBoth
  ) where


import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async, cancel, poll, waitBoth, wait, waitEither)
import           Control.Concurrent.MSem (new, signal)
import qualified Control.Concurrent.MSem as M
import           Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar)
import           Control.Monad.Catch (Exception(..), SomeException, catch, catchAll, finally, throwM)
import           Control.Monad.Loops (untilM_)
import           Control.Monad.Trans.Either (EitherT, pattern EitherT, runEitherT, bimapEitherT)

import qualified Data.Text as T
import           Data.Typeable (Typeable)

import           Entwine.Async (waitEitherBoth)
import           Entwine.Data.Parallel
import           Entwine.Data.Queue
import           Entwine.P

import           System.IO (IO)

-- | Provide a producer and an action to be run across the result
--   of that producer in parallel.
--   For a version that doesn't ignore the results see 'Entwine.Parallel.consume'.
--
--   Common usage:
--
--   @
--     let producer :: Address -> Queue Address -> IO ()
--         producer prefix q =
--           list' prefix $$ writeQueue q
--
--     consume producer 100 (\(a :: Address) -> doThis)
--   @
--
consume_ :: MonadIO m => (Queue b -> IO a) -> Int -> (b -> EitherT e IO ()) -> EitherT (RunError e) m a
consume_ pro fork action = EitherT . liftIO $ do
  q <- newQueue fork

  producer <- async $ pro q

  workers <- emptyWorkers
  result <- newResult $ Right ()
  sem <- new fork

  let spawn = do
       m <- tryReadQueue q
       flip (maybe $ return ()) m $ \a -> do
         w <- do
           M.wait sem
           async $ (runEitherT (action a) >>= void . addResult result . first WorkerError) `finally` signal sem
         addWorker workers w

  let check = do
       threadDelay 1000 {-- 1 ms --}
       p <- poll producer
       e <- isQueueEmpty q
       pure $ isJust p && e

  submitter <- async $ untilM_ spawn check

  -- early termination
  void . async . forever $ do
    threadDelay 1000000 {-- 1 second --}
    getResult result >>=
      either (const $ cancel producer >> cancel submitter) pure

  let waiter = do
        (i, _) <- waitBoth producer submitter
        waitForWorkers workers
        pure i

  (waiter >>= \i -> getResult result >>= pure . second (const i))
    `catchAll` (\z ->
      failWorkers workers >>
        getResult result >>= \w ->
          pure (w >> Left (BlowUpError z)))

data RunError a =
    WorkerError a
  | BlowUpError SomeException
  deriving Show

renderRunError :: RunError a -> (a -> Text) -> Text
renderRunError r render =
  case r of
    WorkerError a ->
      "Worker failed: " <> render a
    BlowUpError e ->
      "An unknown exception was caught: " <> T.pack (show e)

data EarlyTermination =
  EarlyTermination deriving (Eq, Show, Typeable)

instance Exception EarlyTermination

-- | Provide a producer and an action to be run across the results
--   of that producer in parallel and collect the results.
--   For a version that ignores the results see 'Entwine.Parallel.consume_'.
--
--   Common usage:
--
--  @
--     let producer :: Address -> Queue Address -> IO Int
--         producer prefix q =
--           list' prefix $$ writeQueue q
--
--     consume producer 100 (\(a :: Address) -> doThis)
--  @
--
consume :: MonadIO m => (Queue b -> IO a) -> Int -> (b -> IO (Either e c))
        -> m (Either (RunError e) (a, [c]))
consume pro fork action = liftIO . flip catchAll (pure . Left . BlowUpError) $ do
  q <- newQueue fork -- not fork
  producer <- async $ pro q
  workers <- (emptyWorkers :: IO (Workers c))
  sem <- new fork
  early <- newEmptyMVar

  terminator <- async $ takeMVar early

  let spawn :: IO ()
      spawn = do
       m <- tryReadQueue q
       flip (maybe $ return ()) m $ \a -> do
         w <- do
           M.wait sem
           async $ flip finally (signal sem) $ do
             r <- action a
             case r of
               Left e ->
                 putMVar early e >>
                   throwM EarlyTermination
               Right c ->
                 pure $! c
         addWorker workers w

  let check = do
       threadDelay 1000 {-- 1 ms --}
       p <- poll producer
       e <- isQueueEmpty q
       pure $ isJust p && e

  submitter <- async $ untilM_ spawn check

  let waiter = runEitherT $ do
        (i, _) <- bimapEitherT WorkerError id . EitherT $ waitEitherBoth terminator producer submitter

        ws <- liftIO $ getWorkers workers
        ii <- mapM (bimapEitherT WorkerError id . EitherT . waitEither terminator) ws
        pure (i, ii)

  waiter `catch` (\(_ :: EarlyTermination) ->
    failWorkers workers >>
      Left . WorkerError <$> wait terminator)
