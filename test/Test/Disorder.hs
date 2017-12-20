module Test.Disorder (
    testIO
  , testPropertyIO
  , withCPUTime
  ,  disorderMain
  , disorderCliMain
  ) where

import           Control.Monad
import           Control.Monad.IO.Class (liftIO, MonadIO)

import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           System.Directory
import           System.Process
import           System.Exit
import           System.IO
import           System.CPUTime (getCPUTime)

testIO :: Testable a => IO a -> Property
testIO = testPropertyIO . run

testPropertyIO :: Testable a => PropertyM IO a -> Property
testPropertyIO = monadicIO . (=<<) stop

-- | Perform an action and return the CPU time it takes, in picoseconds
-- (actual precision varies with implementation).
withCPUTime :: MonadIO m => m a -> m (Integer, a)
withCPUTime a = do
  t1 <- liftIO getCPUTime
  r <- a
  t2 <- liftIO getCPUTime
  return (t2 - t1, r)

disorderMain :: [IO Bool] -> IO ()
disorderMain tests =
  sanity >> sequence tests >>= \rs -> unless (and rs) exitFailure

disorderCliMain :: [String] -> IO ()
disorderCliMain arguments =
  let ignore p = ".." == p || "." == p || "core" == p
      exec t = callProcess ("test/cli/" ++ t ++ "/run") arguments
   in sanity >> filter (not . ignore) <$> getDirectoryContents "test/cli/" >>= mapM_ exec

sanity :: IO ()
sanity =
  hSetBuffering stdout LineBuffering >> hSetBuffering stderr LineBuffering
