module Main where

import Data.Map
import Network.Wreq (get, Response)
import Data.ByteString.Lazy (ByteString)

-- Access
newtype Read r a = Read { runRead :: r -> a }

getThermostat :: String -> IO (Response ByteString)
getThermostat tstatId =
  get $ host ++ ":" ++ port ++ "/thermostats/" ++ tstatId

getBuildings :: String -> IO (Response ByteString)
getBuildings buildingId =
  get $ host ++ ":" ++ port ++ "/buildings/" ++ buildingId

-- Mutation
newtype Write w a = Write { runWrite :: (w, a) }

-- State
newtype State s a = State { runState :: s -> (a, s) }

main = print "hello"
