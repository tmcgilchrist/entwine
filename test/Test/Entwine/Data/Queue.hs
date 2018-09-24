{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Entwine.Data.Queue where

import           Entwine.P

import           System.IO

import           Test.Disorder
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Entwine.Data.Queue

prop_read_write (a :: Int) = testIO $ do
  q <- newQueue 1
  writeQueue q a
  r <- readQueue q
  pure $ r === a

prop_empty = testIO $ do
  q <- newQueue 1
  e <- isQueueEmpty q
  pure $ e === True

prop_try_read_non_blocking = testIO $ do
  q <- newQueue 1
  m <- tryReadQueue q
  pure $ m === (Nothing :: Maybe Int)

return []
tests :: IO Bool
tests = $quickCheckAll
