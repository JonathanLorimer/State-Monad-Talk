module Memo where

import State (State(..), getState, put)
import qualified Data.Map as M
import Data.Map (Map)

type Memo = Map Int Int

memoFib :: Int -> State Memo Int
memoFib 0 = return 0
memoFib 1 = return 1
memoFib n = undefined

