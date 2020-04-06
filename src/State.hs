{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

module State where

import Network.Wreq (get, Response)
import Data.ByteString.Lazy (ByteString)
import Reader (thermostatIdFromBuilding)

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (State a) = undefined

instance Applicative (State s) where
  pure a = undefined
  (<*>) :: State s (a -> b) -> State s a -> State s b
  State f <*> State a = undefined

instance Monad (State s) where
  return = pure
  (>>=) :: State s a -> (a -> State s b) -> State s b
  State a >>= f = undefined

data AppState = AppState { stateHost :: String
                         , statePort :: String
                         , log :: String }

getState :: State s s
getState = State $ \s -> (s,s)

put :: state -> State state ()
put newState = State $ \_ -> ((), newState)

getThermostat'' :: String -> State AppState (IO (Response ByteString))
getThermostat'' tstatId = undefined
  -- get $ "host" ++ ":" ++ "port" ++ "/thermostats/" ++ tstatId

getBuilding'' :: String -> State AppState (IO (Response ByteString))
getBuilding'' buildingId = undefined
  -- get $ "host" ++ ":" ++ "port" ++ "/buildings/" ++ buildingId


-- getThermostats'' :: String -> State AppState (IO (Response ByteString))
-- getThermostats'' buildingId = pure buildingId
--   >>= getBuilding''
--   >>= getThermostat'' . thermostatIdFromBuilding

-- devState = AppState { stateHost = "localhost"
--                     , statePort = "3008"
--                     , log = "" }

-- prodState = AppState { stateHost = "AWS_IP"
--                      , statePort = "80"
--                      , log = "" }

-- runGetThermostats' :: AppState -> String -> IO (Response ByteString)
-- runGetThermostats' state buildingId = do
--   let (value, AppState{..} ) = runState (getThermostats'' buildingId) state
--   print log
--   value
