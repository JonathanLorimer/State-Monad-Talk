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

getBuildings :: String -> Environment (IO (Response ByteString))
getBuildings buildingId = Read $ \Env {..} ->
  get $ host ++ ":" ++ port ++ "/buildings/" ++ buildingId

-- Mutation
newtype Write w a = Write { runWrite :: (w, a) }

-- State
newtype State s a = State { runState :: s -> (a, s) }

main = print "hello"
