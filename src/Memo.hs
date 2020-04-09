module Memo where

import State (State(..), getState, put)
import qualified Data.Map as M
import Data.Map (Map)

type Memo = Map Int Int

-- memoFib :: Int -> State Memo Int
-- memoFib 0 = return 0
-- memoFib 1 = return 1
-- memoFib n = undefined

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
      m <- getState
      let res = a - b
      put $ M.insert n res m
      return $ a - b

runMemoFib :: Int -> (Int, Memo)
runMemoFib n = runState (memoFib n) M.empty

