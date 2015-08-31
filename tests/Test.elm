module Test (tests) where


import Test.IntDict

import ElmTest.Test exposing (..)


tests : Test
tests =
  suite "elm-intdict test suite"
    [ Test.IntDict.tests
    ]
