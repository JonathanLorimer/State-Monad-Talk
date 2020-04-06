{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE InstanceSigs #-}

module Reader where

import Network.Wreq (get, Response)
import Data.ByteString.Lazy (ByteString)

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f a = undefined

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = undefined
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  Reader f <*> Reader a = undefined

instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  Reader a >>= f = undefined

data Env = Env { host :: String
               , port :: String }

type Environment = Reader Env

ask :: Reader a a
ask = Reader id


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
-- runGetThermostats s e = runReader (getThermostats s) e

-- getDevTstats = runGetThermostats "1234" devEnv
-- getProdTstats = runGetThermostats "1234" prodEnv

