module Test.Route
  ( tests
  ) where

import Prelude

import Action as Action
import Bouzuya.HTTP.Method as Method
import Bouzuya.HTTP.Request.NormalizedPath as NormalizedPath
import Route as Route
import Test.Unit (TestSuite)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = TestUnit.suite "Route" do
  TestUnit.suite "/stopwatches" do
    let path = NormalizedPath.normalize "/stopwatches"

    TestUnit.test "GET" do
      Assert.equal Action.StopwatchList (Route.route path Method.GET)

    TestUnit.test "POST" do
      Assert.equal Action.StopwatchCreate (Route.route path Method.POST)

    TestUnit.test "(other)" do
      Assert.equal
        (Action.MethodNotAllowed [Method.GET, Method.POST])
        (Route.route path Method.PATCH)

  TestUnit.suite "/stopwatches/{id}" do
    let path = NormalizedPath.normalize "/stopwatches/123"

    TestUnit.test "GET" do
      Assert.equal
        (Action.StopwatchGet "123")
        (Route.route path Method.GET)

    TestUnit.test "DELETE" do
      Assert.equal
        (Action.StopwatchDelete "123")
        (Route.route path Method.DELETE)

    TestUnit.test "(other)" do
      Assert.equal
        (Action.MethodNotAllowed [Method.GET, Method.DELETE])
        (Route.route path Method.PATCH)

  TestUnit.suite "/" do
    let path = NormalizedPath.normalize "/"

    TestUnit.test "GET" do
      Assert.equal Action.HealthCheck (Route.route path Method.GET)

    TestUnit.test "(other)" do
      Assert.equal
        (Action.MethodNotAllowed [Method.GET])
        (Route.route path Method.PATCH)

  TestUnit.suite "(other)" do
    let path = NormalizedPath.normalize "/foo"

    TestUnit.test "GET" do
      Assert.equal Action.NotFound (Route.route path Method.GET)
