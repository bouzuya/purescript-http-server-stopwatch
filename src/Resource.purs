module Resource
  ( Stopwatch
  , StopwatchId
  , StopwatchParams
  ) where

type Stopwatch =
  { id :: StopwatchId
  , name :: String
  , created_at :: String
  }

type StopwatchId = String

type StopwatchParams =
  { name :: String
  }
