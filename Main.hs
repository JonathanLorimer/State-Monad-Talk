module Main where

import Data.Map

-- Access
newtype Read r a = Read { runRead :: r -> a }


-- Mutation
newtype Write w a = Write { runWrite :: (w, a) }

-- State
newtype State s a = State { runState :: s -> (a, s) }

main = print "hello"
