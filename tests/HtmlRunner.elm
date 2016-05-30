module Main exposing (..)

import Platform exposing (Program)
import ElmTest exposing (runSuiteHtml)
import Test

main : Program Never
main =
  runSuiteHtml Test.tests
