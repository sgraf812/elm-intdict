module Main where

import Basics exposing (..)
import Signal exposing (..)

import ElmTest.Assertion as A
import ElmTest.Run as R
import ElmTest.Runner.Console exposing (runDisplay)
import ElmTest.Test exposing (..)
import IO.IO exposing (..)
import IO.Runner exposing (Request, Response)
import IO.Runner as Run

import Test.IntDict as IntDict

tests : Test
tests =
    suite "IntDict tests"
    [ IntDict.tests
    ]

console : IO ()
console = runDisplay tests

port requests : Signal Request
port requests = Run.run responses console

port responses : Signal Response