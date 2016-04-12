{-# LANGUAGE DeriveDataTypeable #-}

import Data.Time
import Data.Typeable
import Control.Concurrent
import Control.Exception

data TimedOut = TimedOut UTCTime deriving (Eq, Show, Typeable)
instance Exception TimedOut

timeout :: Int -> IO a -> IO (Maybe a)
timeout usec action = do
  expired <- fmap TimedOut getCurrentTime
  
  ptid <- myThreadId
  let child = do threadDelay usec
                 throwTo ptid expired
      parent = do ctid <- forkIO child
                  result <- action
                  killThread ctid
                  return $ Just result
  catchJust (\e -> if e == expired then Just e else Nothing)
    parent
    (\_ -> return Nothing)

timeout2 :: Int -> IO a -> IO (Maybe a)
timeout2 usec action = do
  expired <- fmap TimedOut getCurrentTime
  
  ptid <- myThreadId
  let child = do threadDelay usec
                 throwTo ptid expired
      parent = bracket (forkIO child) killThread $
               \_ -> fmap Just action
  catchJust (\e -> if e == expired then Just e else Nothing)
    parent
    (\_ -> return Nothing)
