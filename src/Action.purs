module Action
  ( Action(..)
  ) where

import Prelude

import Bouzuya.HTTP.Method (Method)
import Data.String as String
import Resource (StopwatchId)

data Action
  = HealthCheck
  | MethodNotAllowed (Array Method)
  | NotFound
  | StopwatchCreate
  | StopwatchDelete StopwatchId
  | StopwatchGet StopwatchId
  | StopwatchList

derive instance eqAction :: Eq Action

instance showAction :: Show Action where
  show = case _ of
    HealthCheck -> "HealthCheck"
    (MethodNotAllowed methods) ->
      "MethodNotAllowed " <> (String.joinWith ", " (map show methods))
    NotFound -> "NotFound "
    StopwatchCreate -> "StopwatchCreate"
    StopwatchDelete id -> "StopwatchDelete " <> id
    StopwatchGet id -> "StopwatchGet " <> id
    StopwatchList -> "StopwatchList"
