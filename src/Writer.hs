{-# LANGUAGE InstanceSigs #-}

module Writer where

import Network.Wreq (get, Response)
import Data.ByteString.Lazy (ByteString)
import Reader (thermostatIdFromBuilding)

newtype Writer w a = Writer { runWriter :: (w, a) }

instance Functor (Writer w) where
  fmap f = undefined

instance Monoid w => Applicative (Writer w) where
  pure a = Writer $ (mempty, a)
  (<*>) :: Writer w (a -> b) -> Writer w a -> Writer w b
  Writer (w, f) <*> Writer (w', a) = undefined

instance Monoid w => Monad (Writer w) where
  return = pure
  (>>=) :: Writer w a -> (a -> Writer w b) -> Writer w b
  Writer (w, a) >>= f = let (w', b) = runWriter $ f a
                        in Writer (w <> w', b)

type Log = String

makeLog :: String -> String
makeLog endpoint = "GET " ++ endpoint ++ "\n"

getThermostat' :: String -> Writer Log (IO (Response ByteString))
getThermostat' tstatId = undefined
  -- get $ "host" ++ ":" ++ "port" ++ "/thermostats/" ++ tstatId )

getBuilding' :: String -> Writer Log (IO (Response ByteString))
getBuilding' buildingId = undefined
  -- get $ "host" ++ ":" ++ "port" ++ "/buildings/" ++ buildingId )

-- getThermostats' :: String -> Writer Log (IO (Response ByteString))
-- getThermostats' buildingId = pure buildingId
--   >>= getBuilding'
--   >>= getThermostat' . thermostatIdFromBuilding

