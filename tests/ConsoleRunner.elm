module Main exposing (..)

import Platform exposing (Program)
import ElmTest exposing (runSuite)

import Test

main : Program Never
main = runSuite Test.tests
