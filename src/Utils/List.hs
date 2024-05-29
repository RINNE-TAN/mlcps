module Utils.List where

import Data.List (findIndex)

findLastIndex :: (a -> Bool) -> [a] -> Maybe Int
findLastIndex p xs =
  case findIndex p (reverse xs) of
    Just i -> Just (length xs - i - 1)
    Nothing -> Nothing
