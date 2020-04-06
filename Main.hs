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

getThermostat' :: String -> Write Log (IO (Response ByteString))
getThermostat' tstatId = Write $
  ( "GET /thermostats/" ++ tstatId ++ "\n"
  , get $ "host" ++ ":" ++ "port" ++ "/thermostats/" ++ tstatId )

getBuilding' :: String -> Write Log (IO (Response ByteString))
getBuilding' buildingId = Write $
  ( "GET /buildings/" ++ buildingId ++ "\n"
  , get $ "host" ++ ":" ++ "port" ++ "/buildings/" ++ buildingId )

getThermostats' :: String -> Write Log (IO (Response ByteString))
getThermostats' buildingId = pure buildingId
  >>= getBuilding'
  >>= getThermostat' . thermostatIdFromBuilding

-- State
newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f (State a) = State $ \s -> let (a', s') = a s
                                    in (f a', s')

instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  State f <*> State a = State $ \s -> let (f', s') = f s
                                          (a', s'') = a s'
                                       in (f' a', s'')

instance Monad (State s) where
  return = pure
  State a >>= f = State $ \s -> let (a', s') = a s
                                in runState (f a') s'

data AppState = AppState { stateHost :: String
                         , statePort :: String
                         , log :: String }

getState :: State s s
getState = State $ \s -> (s,s)

put :: state -> State state ()
put newState = State $ \_ -> ((), newState)

getThermostat'' :: String -> State AppState (IO (Response ByteString))
getThermostat'' tstatId = do
  AppState {..} <- getState
  let log = log ++ "GET /thermostats/" ++ tstatId ++ "\n"
  put $ AppState {..}
  return $ get $ "host" ++ ":" ++ "port" ++ "/thermostats/" ++ tstatId

getBuilding'' :: String -> State AppState (IO (Response ByteString))
getBuilding'' buildingId = do
  AppState {..} <- getState
  let log = log ++ "GET /buildings/" ++ buildingId ++ "\n"
  put $ AppState {..}
  return $ get $ "host" ++ ":" ++ "port" ++ "/buildings/" ++ buildingId

getThermostats'' :: String -> State AppState (IO (Response ByteString))
getThermostats'' buildingId = pure buildingId
  >>= getBuilding''
  >>= getThermostat'' . thermostatIdFromBuilding

devState = AppState { stateHost = "localhost"
                    , statePort = "3008"
                    , log = "" }

prodState = AppState { stateHost = "AWS_IP"
                     , statePort = "80"
                     , log = "" }

runGetThermostats' :: AppState -> String -> IO (Response ByteString)
runGetThermostats' state buildingId = do
  let (value, AppState{..} ) = runState (getThermostats'' buildingId) state
  print log
  value

-- Memoization Example
type Memo = Map Int Int

memoFib :: Int -> State Memo Int
memoFib 0 = return 0
memoFib 1 = return 1
memoFib n = do
  memo <- getState
  case M.lookup n memo of
    Just x -> return x
    Nothing -> do
      a <- memoFib (n - 1)
      b <- memoFib (n - 2)
      let m = M.insert (n - 1) a m
      let m' = M.insert (n - 2) b m
      put m'
      return $ a - b

main = print "hello"
