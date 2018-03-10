{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Twine.Async where

import           Control.Concurrent.Async (async, wait, cancel)
import           Control.Monad.Catch (catchAll)

import           Control.Monad.Trans.Either
import           Control.Monad.IO.Class (liftIO)

import           Hedgehog

import           Twine.P

import           System.IO

import           Twine.Async
import           Twine.Data
import           Twine.Snooze (snooze)

prop_wait_no_timeout :: Property
prop_wait_no_timeout =
  property $ do
    a <- liftIO . async . snooze $ microseconds 5
    e <- liftIO . runEitherT $ waitWithTimeout a (microseconds 10)
    e === Right ()

prop_wait_timeout :: Property
prop_wait_timeout =
  property $ do
    a <- liftIO . async $ (snooze $ milliseconds 20)
    e <- liftIO . runEitherT $ waitWithTimeout a (milliseconds 5)
    e === Left (AsyncTimeout (milliseconds 5))

prop_cancel_outside :: Property
prop_cancel_outside =
  property $ do
    a <- liftIO . async $ (snooze (milliseconds 20) >> pure True)
    e <- liftIO . async $ runEitherT $ waitWithTimeout a (milliseconds 5)
    liftIO $ cancel a
    r <- liftIO $ wait e `catchAll` (\_ -> pure . Right $ False)
    r === Right False

tests :: IO Bool
tests =
  checkSequential $$(discover)
