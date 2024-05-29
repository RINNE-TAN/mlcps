module Utils.Env where

import Control.Monad.State (State, evalState, gets, modify)
import qualified Data.HashMap.Internal.Strict as H
import Data.Hashable (Hashable)

type EnvM k v = State (H.HashMap k v)

insert :: (Hashable k) => k -> v -> EnvM k v ()
insert key value = modify (H.insert key value)

lookup :: (Hashable k) => k -> EnvM k v (Maybe v)
lookup key = gets (H.lookup key)

runEnv :: (Hashable k) => EnvM k v a -> [(k, v)] -> a
runEnv transM l = evalState transM (H.fromList l)