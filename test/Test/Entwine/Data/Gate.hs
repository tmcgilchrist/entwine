{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Entwine.Data.Gate where

import           Test.Disorder (testIO)

import           Entwine.P

import           Test.QuickCheck

import           Entwine.Data.Gate

--
-- A new gate should isOpen
--

prop_isOpen = once . testIO $
  newGate >>=
    isOpen >>=
      pure . (===) True


--
-- A gate that has been closed should not be open
--

prop_close = once . testIO $
  newGate >>= \c ->
    close c >>
      isOpen c >>=
        pure . (===) False


--
-- A gate can be closed multiple times
--

prop_close_multiple = once . testIO $
  newGate >>= \c ->
    close c >> close c >> pure True

return []
tests = $quickCheckAll
