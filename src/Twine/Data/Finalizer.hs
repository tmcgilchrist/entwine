{-# LANGUAGE NoImplicitPrelude #-}
module Twine.Data.Finalizer (
    Finalizer (..)
  ) where

import           Twine.P

import           System.IO


-- |
-- A callback which can be invoked to cleanup a resource
--
newtype Finalizer =
  Finalizer {
      finalize :: IO ()
    }

instance Semigroup Finalizer where
  {-# INLINE (<>) #-}
  (<>) (Finalizer a) (Finalizer b) =
    Finalizer $ a >> b

instance Monoid Finalizer where
  {-# INLINE mempty #-}
  mempty =
    Finalizer $ pure ()

  {-# INLINE mappend #-}
  mappend (Finalizer a) (Finalizer b) =
    Finalizer $ a >> b
