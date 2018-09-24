{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Entwine.Parallel where

import           Control.Concurrent.MVar
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either

import           Test.Disorder (testIO)

import           Entwine.P

import           System.IO
import           System.IO.Error

import           Test.QuickCheck

import           Entwine.Data
import           Entwine.Parallel

data Fail =
  IFailed

prop_consume = forAll (choose (1, 10 :: Int)) $ \n -> testIO $ do
  r <- newMVar ([] :: [Int])

  let pro = \q -> forM_ [1..n] (writeQueue q)

      work :: Int -> EitherT Fail IO ()
      work i =
        liftIO $ modifyMVar_ r (\l -> pure $ i : l)

  _ <- runEitherT $ consume_ pro 1 work

  g <- readMVar r

  pure $ length g === n

prop_worker_fail = forAll (choose (1, 1000 :: Int)) $ \n -> testIO $ do
  let pro = \q -> forM_ [1..n] (writeQueue q)
      work = const $ left IFailed

  r <- runEitherT $ consume_ pro 1 work
  z <- pure $ case r of
    (Left (WorkerError IFailed)) ->
      True
    _ ->
      False
  pure $ z === True

prop_worker_blow_up_worker = forAll (choose (1, 1000 :: Int)) $ \n -> testIO $ do
  let pro = \q -> forM_ [1..n] (writeQueue q)
      work = const . throwM . userError $ "what is this?"

  r <- runEitherT $ consume_ pro 1 work
  z <- pure $ case r of
    (Left (BlowUpError _)) ->
      True
    _ ->
      False
  pure $ z === True

prop_worker_blow_up_producer = testIO $ do
  let pro = const . throwM . userError $ "producer"
      work = const $ pure ()

  r <- runEitherT $ consume_ pro 1 work
  z <- pure $ case r of
    (Left (BlowUpError _)) ->
      True
    _ ->
      False
  pure $ z === True

prop_empty_producer = testIO $ do
  let pro = const $ pure ()
      work = const $ pure ()

  r <- runEitherT $ consume_ pro 1 work
  pure $ (isRight r) === True


return []
tests :: IO Bool
tests = $quickCheckAll
