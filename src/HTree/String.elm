module HTree.String exposing (parse, toString, toOutline, level)


{-|  HTree.String has some convenience functions for dealing with
outlines and trees whose nodes are labeled by strings.


@docs parse, toString, toOutline, level

-}

import HTree
import Tree exposing(Tree)
import Parser exposing(Parser, succeed, chompWhile, getChompedString, (|.))

{-|

    > import HTree.String exposing(..)
    > import Example.Test exposing(..)

    o3 = -- from Example.Test
    A
      p
        1
        2
        3
      q
    B
      r
      s
    C

    > t = parse o3
      Tree "*" [
         Tree "A" [Tree "p" [Tree "1" [],Tree "2" [],Tree "3" []],Tree "q" []]
        ,Tree "B" [Tree "r" [],Tree "s" []]
        ,Tree "C" []]

    > t = parse o3 |> HTree.tag
      Tree ("*",0) [
         Tree ("A",1) [Tree ("p",2) [Tree ("1",3) [],Tree ("2",3) [],Tree ("3",3) []]],Tree ("q",1) []
        ,Tree ("B",1) [Tree ("r",2) [],Tree ("s",2) []]
        ,Tree ("C",1) []]

-}
parse : String -> Tree String
parse str =
    String.split "\n" str
      |> List.filter (\s -> s /= "")
      |> HTree.fromList "*" level
      |> Tree.map (\l -> String.trim l)


{-| Compute the string representation of a tree labeled by strings

    > toString t
    "*\nA\n  p\n    1\n    2\n    3\n  q\nB\n  r\n  s\nC" : String

-}
toString : Tree String -> String
toString t =
     HTree.toOutline identity t


{-| Compute the string representation of a tree labeled by strings;
omit the root label

    > toOutline t
    "A\n  p\n    1\n    2\n    3\n  q\nB\n  r\n  s\nC" : String
-}
toOutline : Tree String -> String
toOutline t =
    String.dropLeft 2 (toString t)


{-| Compute the level of a string: the number of
leading spaces, divided by 2
-}
level : String -> Int
level str =
    Parser.run numberOfLeadingBlanks str
      |> Result.toMaybe
      |> Maybe.map (\l -> 1 + l//2)
      |> Maybe.withDefault 0


numberOfLeadingBlanks : Parser Int
numberOfLeadingBlanks =
    (succeed ()
        |. chompWhile (\c -> c == ' ')
    )
        |> getChompedString
        |> Parser.map String.length


