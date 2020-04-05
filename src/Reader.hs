{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE InstanceSigs #-}

module Reader where

newtype Read r a = Read { runRead :: r -> a }

instance Functor (Read r) where
  fmap :: (a -> b) -> Read r a -> Read r b
  fmap f a = undefined

instance Applicative (Read r) where
  pure :: a -> Read r a
  pure a = undefined
  (<*>) :: Read r (a -> b) -> Read r a -> Read r b
  Read f <*> Read a = undefined

instance Monad (Read r) where
  return = pure
  (>>=) :: Read r a -> (a -> Read r b) -> Read r b
  Read a >>= f = undefined

data Env = Env { host :: String
               , port :: String }

type Environment = Read Env

ask :: Read a a
ask = Read id


getThermostat :: String -> Environment (IO (Response ByteString))
getThermostat tstatId = undefined
  -- (get $ host ++ ":" ++ port ++ "/thermostats/" ++ tstatId)

getBuilding :: String -> Environment (IO (Response ByteString))
getBuilding buildingId = undefined
  -- get $ host ++ ":" ++ port ++ "/buildings/" ++ buildingId

thermostatIdFromBuilding :: IO (Response ByteString) -> String
thermostatIdFromBuilding = undefined

devEnv = Env { host = "localhost"
             , port = "3008" }

prodEnv = Env { host = "AWS_IP_ADDRESS"
              , port = "80" }

type M = Environment
type A = String
type B = IO (Response ByteString)

-- getThermostat :: A -> M B
-- getBuildings :: A -> M B

-- getThermostats :: String -> Environment (IO (Response ByteString))
-- getThermostats buildingId = pure buildingId
--   >>= getBuilding
--   >>= getThermostat . thermostatIdFromBuilding

-- runGetThermostats :: String -> Env -> IO (Response ByteString)
-- runGetThermostats s e = runRead (getThermostats s) e

-- getDevTstats = runGetThermostats "1234" devEnv
-- getProdTstats = runGetThermostats "1234" prodEnv

