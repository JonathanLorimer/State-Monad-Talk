module Main where

import Data.Map

newtype ReadIt r a = ReadIt { unRead :: r -> a }

main = print "hello"
