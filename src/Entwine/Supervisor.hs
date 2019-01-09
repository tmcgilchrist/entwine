{-
  This file is copyright (c) 2014 Alfredo Di Napoli (https://github.com/adinapoli/threads-supervisor)
  under an MIT License.
  Used under the three-clause BSD license found in the LICENSE file in the entwine root.
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
  Supervisor is a Simple IO-based library for Erlang-style thread supervision.
  Minor adjustments made to remove unbounded queue usage and use entwine versions of
  queue abstractions.
-}
module Entwine.Supervisor
  ( SupervisionCtx
  , Supervisor
  , Child_
  , DeadLetter
  , RestartAction
  , SupervisionEvent(..)
  , RestartStrategy(..)
  , RestartResult(..)
  -- * Creating a new supervisor
  -- $new
  , newSupervisor
  -- * Restart Policies
  , fibonacciRetryPolicy
  -- * Stopping a supervisor
  -- $shutdown
  , shutdownSupervisor
  -- * Accessing Supervisor event log
  -- $log
  , eventStream
  , activeChildren
  -- * Supervise a forked thread
  -- $fork
  , forkSupervised
  -- * Monitor another supervisor
  -- $monitor
  , monitorWith
  ) where


import           Control.Concurrent (ThreadId, killThread, forkFinally, threadDelay, myThreadId)
import           Control.Concurrent.STM (TChan, atomically, writeTChan, readTChan, dupTChan, newTChanIO)

import           Control.Exception (SomeException, AsyncException, bracket, asyncExceptionFromException)
import           Control.Retry (RetryPolicyM (..), RetryStatus (..), defaultRetryStatus, fibonacciBackoff)

import qualified Data.HashMap.Strict as Map
import           Data.IORef (IORef, newIORef, writeIORef, readIORef, atomicModifyIORef')
import           Data.Time (UTCTime, getCurrentTime)

import           Entwine.P
import           Entwine.Data.Queue

import           Numeric.Natural (Natural)

import           System.Clock (Clock (Monotonic), TimeSpec, getTime)
import           System.IO (IO)

--------------------------------------------------------------------------------
type Mailbox = TChan DeadLetter

--------------------------------------------------------------------------------
data SupervisionCtx q = SupervisionCtx {
    _sc_mailbox          :: Mailbox
  , _sc_parent_mailbox   :: !(IORef (Maybe Mailbox))
  -- ^ The mailbox of the parent process (which is monitoring this one), if any.
  , _sc_children         :: !(IORef (Map.HashMap ThreadId (Child_ q)))
  , _sc_eventStream      :: q SupervisionEvent
  , _sc_eventStreamSize  :: !Natural
  , _sc_strategy         :: !RestartStrategy
  }

--------------------------------------------------------------------------------
data Supervisor q = Supervisor {
    _sp_myTid          :: !ThreadId
  , _sp_ctx            :: !(SupervisionCtx q)
  }

--------------------------------------------------------------------------------
data DeadLetter = DeadLetter !LetterEpoch !ThreadId !SomeException

--------------------------------------------------------------------------------
type Epoch = TimeSpec
newtype LetterEpoch = LetterEpoch Epoch deriving Show
newtype ChildEpoch  = ChildEpoch  Epoch deriving Show

--------------------------------------------------------------------------------
data RestartResult =
    Restarted !ThreadId !ThreadId !RetryStatus !UTCTime
    -- ^ The supervised `Child_` was restarted successfully.
  | StaleDeadLetter !ThreadId !LetterEpoch !ChildEpoch !UTCTime
    -- ^ A stale `DeadLetter` was received.
  | RestartFailed SupervisionEvent
    -- ^ The restart failed for a reason decribed by a `SupervisionEvent`
  deriving Show

--------------------------------------------------------------------------------
data Child_ q = Worker !ChildEpoch !RetryStatus (RetryPolicyM IO) RestartAction
              | Supvsr !ChildEpoch !RetryStatus (RetryPolicyM IO) !(Supervisor q)

--------------------------------------------------------------------------------
type RestartAction = ThreadId -> IO ThreadId

--------------------------------------------------------------------------------
data SupervisionEvent =
     ChildBorn !ThreadId !UTCTime
   | ChildDied !ThreadId !SomeException !UTCTime
   | ChildRestarted !ThreadId !ThreadId !RetryStatus !UTCTime
   | ChildNotFound  !ThreadId !UTCTime
   | StaleDeadLetterReceived  !ThreadId !LetterEpoch !ChildEpoch !UTCTime
   | ChildRestartLimitReached !ThreadId !RetryStatus !UTCTime
   | ChildFinished !ThreadId !UTCTime
   deriving Show

--------------------------------------------------------------------------------
-- | Erlang inspired strategies. At the moment only the 'OneForOne' is
-- implemented.
data RestartStrategy
  = OneForOne
  deriving Show

--------------------------------------------------------------------------------
-- | Smart constructor which offers a default throttling based on
-- fibonacci numbers.
fibonacciRetryPolicy :: RetryPolicyM IO
fibonacciRetryPolicy = fibonacciBackoff 100

--------------------------------------------------------------------------------
getEpoch :: MonadIO m => m Epoch
getEpoch = liftIO $ getTime Monotonic

--------------------------------------------------------------------------------
tryNotifyParent :: IORef (Maybe Mailbox) -> ThreadId -> SomeException -> IO ()
tryNotifyParent mbPMbox myId ex = do
  readIORef mbPMbox >>= \m -> case m of
    Nothing -> pure ()
    Just m' -> do
      e <- getEpoch
      atomically $ writeTChan m' (DeadLetter (LetterEpoch e) myId ex)

-- $new
-- In order to create a new supervisor, you need a `SupervisorSpec`,
-- which can be acquired by a call to `newSupervisor`:

-- $supervise

--------------------------------------------------------------------------------
newSupervisor :: RestartStrategy
              -> Natural
              -> IO (Supervisor Queue)
newSupervisor strategy size = do
  parentMbx <- newIORef Nothing
  es  <- newQueue size
  mbx <- newTChanIO   -- TODO Unbounded queue might not be the best idea!
  cld <- newIORef Map.empty
  let ctx = SupervisionCtx {
          _sc_mailbox         = mbx
        , _sc_parent_mailbox  = parentMbx
        , _sc_eventStream     = es
        , _sc_children        = cld
        , _sc_strategy        = strategy
        , _sc_eventStreamSize = size
        }
  tid <- forkFinally (handleEvents ctx) $ \res -> case res of
    Left ex -> do
      bracket myThreadId pure $ \myId -> do
        -- If we have a parent supervisor watching us, notify it we died.
        tryNotifyParent parentMbx myId ex
    Right v -> pure v
  go ctx tid
  where
    go ctx tid = do
      pure Supervisor {
        _sp_myTid = tid
      , _sp_ctx   = ctx
      }

-- $log

--------------------------------------------------------------------------------
-- | Gives you access to the event this supervisor is generating, allowing you
-- to react. It's using a bounded queue to explicitly avoid memory leaks in case
-- you do not want to drain the queue to listen to incoming events.
eventStream :: Supervisor Queue -> Queue SupervisionEvent
eventStream Supervisor{_sp_ctx} = _sc_eventStream _sp_ctx

--------------------------------------------------------------------------------
-- | Returns the number of active threads at a given moment in time.
activeChildren :: Supervisor Queue -> IO Int
activeChildren Supervisor{_sp_ctx} = do
  readIORef (_sc_children _sp_ctx) >>= pure . length . Map.keys

-- $shutdown

--------------------------------------------------------------------------------
-- | Shutdown the given supervisor. This will cause the supervised children to
-- be killed as well. To do so, we explore the children tree, killing workers as we go,
-- and recursively calling `shutdownSupervisor` in case we hit a monitored `Supervisor`.
shutdownSupervisor :: Supervisor Queue -> IO ()
shutdownSupervisor (Supervisor tid ctx) = do
  chMap <- readIORef (_sc_children ctx)
  processChildren (Map.toList chMap)
  killThread tid
  where
    processChildren [] = pure ()
    processChildren (x:xs) = do
      case x of
        (workerTid, Worker{}) -> killThread workerTid
        (_, Supvsr _ _ _ s) -> shutdownSupervisor s
      processChildren xs

-- $fork

--------------------------------------------------------------------------------
-- | Fork a thread in a supervised mode.
forkSupervised :: Supervisor Queue
               -- ^ The 'Supervisor'
               -> RetryPolicyM IO
               -- ^ The retry policy to use
               -> IO ()
               -- ^ The computation to run
               -> IO ThreadId
forkSupervised sup policy act =
  bracket (supervised sup act) pure $ \newChild -> do
    e <- getEpoch
    let ch = Worker (ChildEpoch e) defaultRetryStatus policy (const (supervised sup act))
    atomicModifyIORef' (_sc_children (_sp_ctx sup)) $ \chMap -> (Map.insert newChild ch chMap, ())
    now <- getCurrentTime
    writeQueue (_sc_eventStream (_sp_ctx sup)) (ChildBorn newChild now)
    pure newChild

--------------------------------------------------------------------------------
supervised :: Supervisor Queue -> IO () -> IO ThreadId
supervised sup act = forkFinally act $ \res -> case res of
  Left ex -> bracket myThreadId pure $ \myId -> do
    e <- getEpoch
    atomically $ writeTChan (_sc_mailbox (_sp_ctx sup)) (DeadLetter (LetterEpoch e) myId ex)
  Right _ -> bracket myThreadId pure $ \myId -> do
    now <- getCurrentTime
    atomicModifyIORef' (_sc_children (_sp_ctx sup)) $ \chMap -> (Map.delete myId chMap, ())
    writeQueue (_sc_eventStream (_sp_ctx sup)) (ChildFinished myId now)

--------------------------------------------------------------------------------
-- | Ignore any stale `DeadLetter`, which is a `DeadLetter` with an `Epoch`
-- smaller than the one stored in the `Child_` to restart. Such stale `DeadLetter`
-- are simply ignored.
ignoringStaleLetters :: ThreadId
                     -> LetterEpoch
                     -> ChildEpoch
                     -> IO RestartResult
                     -> IO RestartResult
ignoringStaleLetters tid deadLetterEpoch@(LetterEpoch l) childEpoch@(ChildEpoch c) act = do
  now <- getCurrentTime
  if l < c then pure (StaleDeadLetter tid deadLetterEpoch childEpoch now) else act

--------------------------------------------------------------------------------
restartChild :: SupervisionCtx Queue
             -> LetterEpoch
             -> UTCTime
             -> ThreadId
             -> IO RestartResult
restartChild ctx deadLetterEpoch now newDeath = do
  chMap <- readIORef (_sc_children ctx)
  case Map.lookup newDeath chMap of
    Nothing -> pure $ RestartFailed (ChildNotFound newDeath now)
    Just (Worker workerEpoch rState rPolicy act) -> ignoringStaleLetters newDeath deadLetterEpoch workerEpoch $ do
      runRetryPolicy rState rPolicy emitEventChildRestartLimitReached $ \newRState -> do
        e <- getEpoch
        let ch = Worker (ChildEpoch e) newRState rPolicy act
        newThreadId <- act newDeath
        writeIORef (_sc_children ctx) (Map.insert newThreadId ch $! Map.delete newDeath chMap)
        emitEventChildRestarted newThreadId newRState
    Just (Supvsr supervisorEpoch rState rPolicy (Supervisor deathSup ctx')) -> do
      ignoringStaleLetters newDeath deadLetterEpoch supervisorEpoch $ do
        runRetryPolicy rState rPolicy emitEventChildRestartLimitReached $ \newRState -> do
          e <- getEpoch
          restartedSup <- newSupervisor (_sc_strategy ctx) (_sc_eventStreamSize ctx')
          let ch = Supvsr (ChildEpoch e) newRState rPolicy restartedSup
          -- TODO: shutdown children?
          let newThreadId = _sp_myTid restartedSup
          writeIORef (_sc_children ctx) (Map.insert newThreadId ch $! Map.delete deathSup chMap)
          emitEventChildRestarted newThreadId newRState
  where
    emitEventChildRestarted newThreadId newRState = do
      pure $ Restarted newDeath newThreadId newRState now
    emitEventChildRestartLimitReached newRState = do
      pure $ RestartFailed (ChildRestartLimitReached newDeath newRState now)
    runRetryPolicy :: RetryStatus
                   -> RetryPolicyM IO
                   -> (RetryStatus -> IO RestartResult)
                   -> (RetryStatus -> IO RestartResult)
                   -> IO RestartResult
    runRetryPolicy rState rPolicy ifAbort ifThrottle = do
     maybeDelay <- getRetryPolicyM rPolicy rState
     case maybeDelay of
       Nothing -> ifAbort rState
       Just delay ->
         let newRState = rState { rsIterNumber = rsIterNumber rState + 1
                                , rsCumulativeDelay = rsCumulativeDelay rState + delay
                                , rsPreviousDelay = Just (maybe 0 (const delay) (rsPreviousDelay rState))
                                }
         in threadDelay delay >> ifThrottle newRState

--------------------------------------------------------------------------------
restartOneForOne :: SupervisionCtx Queue
                 -> LetterEpoch
                 -> UTCTime
                 -> ThreadId
                 -> IO RestartResult
restartOneForOne = restartChild

--------------------------------------------------------------------------------
handleEvents :: SupervisionCtx Queue -> IO ()
handleEvents ctx = do
  (DeadLetter epoch newDeath ex) <- atomically $ readTChan (_sc_mailbox ctx)
  now <- getCurrentTime
  writeQueue (_sc_eventStream ctx) (ChildDied newDeath ex now)
  -- If we catch an `AsyncException`, we have nothing but good
  -- reasons NOT to restart the thread.
  case asyncExceptionFromException ex of
    Just (_ :: AsyncException) -> do
      -- Remove the `Child_` from the map, log what happenend.
      atomicModifyIORef' (_sc_children ctx) $ \chMap -> (Map.delete newDeath chMap, ())
      writeQueue (_sc_eventStream ctx) (ChildDied newDeath ex now)
      handleEvents ctx
    Nothing -> do
      restartResult <- case (_sc_strategy ctx) of
        OneForOne -> restartOneForOne ctx epoch now newDeath
      -- TODO: shutdown supervisor?
      case restartResult of
        StaleDeadLetter tid le we tm -> do
          writeQueue (_sc_eventStream ctx) (StaleDeadLetterReceived tid le we tm)
        RestartFailed reason -> do
          writeQueue (_sc_eventStream ctx) reason
        Restarted oldId newId rStatus tm ->
          writeQueue (_sc_eventStream ctx) (ChildRestarted oldId newId rStatus tm)
      handleEvents ctx

-- $monitor

--------------------------------------------------------------------------------
-- | Monitor another supervisor. To achieve these, we simulate a new 'DeadLetter',
-- so that the first supervisor will effectively restart the monitored one.
-- Thanks to the fact that for the supervisor the restart means we just copy over
-- its internal state, it should be perfectly fine to do so.
-- Returns the `ThreadId` of the monitored supervisor.
monitorWith :: RetryPolicyM IO
            -- ^ The retry policy to use
            -> Supervisor q
            -- ^ The supervisor
            -> Supervisor q
            -- ^ The 'supervised' supervisor
            -> IO ThreadId
monitorWith policy sup1 sup2 = do
  let sup1Children = _sc_children (_sp_ctx sup1)
  let sup1Mailbox  = _sc_mailbox (_sp_ctx sup1)
  let sup2Id = _sp_myTid sup2
  let sup2ParentMailbox = _sc_parent_mailbox (_sp_ctx sup2)

  readIORef sup2ParentMailbox >>= \mbox -> case mbox of
    Just _ -> pure sup2Id -- Do nothing, this supervisor is already being monitored.
    Nothing -> do
      e <- getEpoch
      let sup2RetryStatus = defaultRetryStatus
      let ch' = Supvsr (ChildEpoch e) sup2RetryStatus policy sup2
      atomicModifyIORef' sup1Children $ \chMap -> (Map.insert sup2Id ch' chMap, ())
      duped <- atomically $ dupTChan sup1Mailbox
      atomicModifyIORef' sup2ParentMailbox $ const (Just duped, ())
      pure sup2Id
