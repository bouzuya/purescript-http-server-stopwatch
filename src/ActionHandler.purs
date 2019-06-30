module ActionHandler
  ( handler
  ) where

import Prelude

import Action as Action
import Bouzuya.DateTime.Formatter.DateTime as DateTimeFormatter
import Bouzuya.HTTP.Body as Body
import Bouzuya.HTTP.Request (Request)
import Bouzuya.HTTP.Request.NormalizedPath as NormalizedPath
import Bouzuya.HTTP.Response (Response)
import Bouzuya.HTTP.StatusCode as StatusCode
import Bouzuya.UUID.V4 as UUIDv4
import Data.Array as Array
import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.Int as Int
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.String.CodeUnits as CodeUnits
import Data.Time.Duration (Seconds(..))
import Effect.Aff (Aff)
import Effect.Class as Class
import Effect.Class.Console as Console
import Effect.Now as Now
import Math as Math
import Partial.Unsafe as Unsafe
import Record as Record
import Resource (Stopwatch, StopwatchParams, StopwatchId)
import ResponseHelper as ResponseHelper
import Route as Route
import Simple.JSON as SimpleJSON
import Store (Store)
import Store as Store

handler :: Store (Array Stopwatch) -> Request -> Aff Response
handler store { body, method, pathname } = do
  Console.log ((show method) <> " " <> pathname)
  let
    normalized = NormalizedPath.normalize pathname
    normalizedPathname = NormalizedPath.toString normalized
  if pathname /= normalizedPathname
    then ResponseHelper.status301 normalizedPathname
    else do
      let action = Route.route normalized method
      case action of
        Action.StopwatchCreate -> do
          body' <- Class.liftEffect (Body.fromArray body) -- TODO
          case (SimpleJSON.readJSON_ body' :: _ StopwatchParams) of
            Maybe.Nothing ->
              -- TODO: message
              ResponseHelper.fromStatus StatusCode.status400 []
            Maybe.Just params -> do
              stopwatch <- create store params
              ResponseHelper.fromJSON stopwatch
        Action.StopwatchDelete id -> do
          _ <- delete store id
          ResponseHelper.status204
        Action.StopwatchGet id -> do
          stopwatchMaybe <- get store id
          case stopwatchMaybe of
            Maybe.Nothing -> ResponseHelper.status404
            Maybe.Just stopwatch -> do
              case
                do
                  s <-
                    CodeUnits.slice
                      0
                      (CodeUnits.length "YYYY-MM-DDTHH:MM:SS")
                      stopwatch.created_at
                  DateTimeFormatter.fromString s of
                Maybe.Nothing -> ResponseHelper.status500
                Maybe.Just start -> do
                  end <- Class.liftEffect Now.nowDateTime
                  let (Seconds elapsedTime) = DateTime.diff end start :: Seconds
                  ResponseHelper.fromJSON
                    (Record.merge
                      stopwatch
                      { elaplased_time:
                          Unsafe.unsafePartial
                            (Maybe.fromJust
                              (Int.fromNumber (Math.floor elapsedTime))) })
        Action.StopwatchList -> do
          stopwatches <- list store
          ResponseHelper.fromJSON stopwatches
        Action.HealthCheck ->
          ResponseHelper.fromStatus StatusCode.status200 []
        Action.MethodNotAllowed methods ->
          ResponseHelper.status405 methods
        Action.NotFound ->
          ResponseHelper.status404

create :: Store (Array Stopwatch) -> StopwatchParams -> Aff Stopwatch
create store params = do
  id <- Class.liftEffect (map UUIDv4.toString UUIDv4.generate)
  created_at <- Class.liftEffect (map dateTimeToString Now.nowDateTime)
  let stopwatch = Record.merge params { created_at, id }
  stopwatches <- Store.get store
  let stopwatches' = Array.insert stopwatch stopwatches
  _ <- Store.put stopwatches' store
  pure stopwatch

delete :: Store (Array Stopwatch) -> StopwatchId -> Aff (Maybe Unit)
delete store id = do
  stopwatches <- Store.get store
  case Array.findIndex ((eq id) <<< _.id) stopwatches of
    Maybe.Nothing -> pure Maybe.Nothing
    Maybe.Just index -> do
      case Array.deleteAt index stopwatches of
        Maybe.Nothing -> pure Maybe.Nothing
        Maybe.Just stopwatches' -> do
          _ <- Store.put stopwatches' store
          pure (Maybe.Just unit)

get :: Store (Array Stopwatch) -> StopwatchId -> Aff (Maybe Stopwatch)
get store id = do
  stopwatches <- Store.get store
  pure (Array.find ((eq id) <<< _.id) stopwatches)

list :: Store (Array Stopwatch) -> Aff (Array Stopwatch)
list = Store.get

dateTimeToString :: DateTime -> String
dateTimeToString dt = DateTimeFormatter.toString dt <> "Z"
