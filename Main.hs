{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Data.Map
import Network.Wreq (get, Response)
import Data.ByteString.Lazy (ByteString)
import Prelude hiding (Read)

-- Access
newtype Read r a = Read { runRead :: r -> a }

instance Functor (Read r) where
  fmap :: (a -> b) -> Read r a -> Read r b
  fmap f a = Read $ f . (runRead a)

instance Applicative (Read r) where
  pure :: a -> Read r a
  pure a = Read $ \r -> a
  (<*>) :: Read r (a -> b) -> Read r a -> Read r b
  Read f <*> Read a = Read $ \r -> f r $ a r

instance Monad (Read r) where
  return = pure
  (>>=) :: Read r a -> (a -> Read r b) -> Read r b
  Read a >>= f = Read $ \r -> runRead (f $ a r) r

data Env = Env { host :: String
               , port :: String }

type Environment = Read Env

ask :: Read a a
ask = Read id

type M = Environment
type A = String
type B = IO (Response ByteString)

-- getThermostat :: A -> M B
-- getBuildings :: A -> M B

getThermostat :: String -> Environment (IO (Response ByteString))
getThermostat tstatId = do
  (Env {..}) <- ask
  return (get $ host ++ ":" ++ port ++ "/thermostats/" ++ tstatId)

getBuilding :: String -> Environment (IO (Response ByteString))
getBuilding buildingId = Read $ \Env {..} ->
  get $ host ++ ":" ++ port ++ "/buildings/" ++ buildingId

thermostatIdFromBuilding :: IO (Response ByteString) -> String
thermostatIdFromBuilding = undefined

devEnv = Env { host = "localhost"
             , port = "3008" }

prodEnv = Env { host = "AWS_IP_ADDRESS"
              , port = "80" }

getThermostats :: String -> Environment (IO (Response ByteString))
getThermostats buildingId = pure buildingId
  >>= getBuilding
  >>= getThermostat . thermostatIdFromBuilding

runGetThermostats :: String -> Env -> IO (Response ByteString)
runGetThermostats s e = runRead (getThermostats s) e

getDevTstats = runGetThermostats "1234" devEnv
getProdTstats = runGetThermostats "1234" prodEnv

-- Mutation
newtype Write w a = Write { runWrite :: (w, a) }

instance Functor (Write w) where
  fmap f = Write . fmap f . runWrite

instance Monoid w => Applicative (Write w) where
  pure a = Write $ (mempty, a)
  Write (w, f) <*> Write (w', a) = Write $ (w <> w', f a)

instance Monoid w => Monad (Write w) where
  return = pure
  Write (w, a) >>= f = let (w', b) = runWrite $ f a
                        in Write (w <> w', b)

type Log = String

getThermostat' :: String -> (Log, IO (Response ByteString))
getThermostat' tstatId = undefined

getBuilding' :: String -> (Log, IO (Response ByteString))
getBuilding' buildingId = undefined

-- State
newtype State s a = State { runState :: s -> (a, s) }

main = print "hello"
