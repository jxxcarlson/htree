module HTreeTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import HTree as HT
import HTree.String as HS
import Example.Test as Example

suite : Test
suite =
    describe "The HTree module"
        [ lineTests

        ]

{-| Find the tree representation of a string, convert it back to
a string, then see if the result is the same as as the original
string, modulo trimming strings.
-}
identityTest str =
    str
      |> HS.parse
      |> HS.toOutline
      |> Expect.equal (String.trim str)


lineTests =
    describe "OutlineReader.tree"
        [ test "o1" <|
            \_ ->
               identityTest Example.o1
          , test "o2" <|
            \_ ->
               identityTest Example.o2
          , test "o3" <|
            \_ ->
               identityTest Example.o3
         , test "o4" <|
            \_ ->
               identityTest Example.o4
         , test "o4y" <|
            \_ ->
               identityTest Example.o4y
         , test "o4z" <|
            \_ ->
               identityTest Example.o4z
         , test "o4w" <|
            \_ ->
               identityTest Example.o4w

        ]
