{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Twine.Guard where

import           Control.Concurrent
import           Control.Monad.Catch
import           Control.Monad.Trans.Either

import           Test.Disorder

import           Twine.P

import           System.IO
import           System.IO.Error

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Twine.Guard

data HResult =
  HExplosion | HError Text | HGraceful deriving (Eq, Show)

handler v = TerminationHandler {
    onExplosion = const $ putMVar v HExplosion >> pure Die
  , onError = \t -> putMVar v (HError t) >> pure Die
  , onGraceful = putMVar v HGraceful >> pure Die
  }

prop_guard_graceful =
  testIO $ do
    v <- newEmptyMVar
    withThread v (pure ()) $ do
      r <- takeMVar v
      pure $ r === HGraceful

prop_guard_error t =
  testIO $ do
    v <- newEmptyMVar
    withThread v (left t) $ do
      r <- takeMVar v
      pure $ r === HError t

prop_guard_explosion =
  testIO $ do
    v <- newEmptyMVar
    withThread v (throwM $ userError "explodez") $ do
      r <- takeMVar v
      pure $ r === HExplosion

withThread :: MVar HResult -> EitherT Text IO () -> IO a -> IO a
withThread v action =
  bracket (forkIO $ guarded (handler v) action) (killThread) . const

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 3 })
