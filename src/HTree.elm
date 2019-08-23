module HTree exposing (fromList, flatten, toOutline, tagWithDepth, depth, nodeCount)

{-| Convert hierarchical lists to a rose tree,
convert a rose tree to a list of node labels,
convert a rose tree to a string representing the
corresponding outline, etc. See HTreeExample.elm.

@docs fromList, flatten, toOutline, tagWithDepth, depth, nodeCount

-}

import Tree exposing (Tree, singleton)
import Tree.Zipper as Zipper exposing (Zipper)


{-| Suppose given:

  - a root element of type a
  - a function that determines the level of an item
  - a list of items of type a

Then `fromList` returns the corresponding rose tree.
For an example, consider the outline given by the string
\`Example.Test.o2:

    A
        p
        q

    B
        r
        s

    C

Then we have

    > import HTree exposing(..)
    > import HTree.String as HS
    > import Example.Test as Example

    > data = String.split "\n" o2 |> List.filter (\line -> line /= "")
    ["A","  p","  q","B","  r","  s","C"]

    > t = fromList "*" HS.level data
    Tree "*"
       [Tree "A" [Tree ("  p") [],Tree ("  q") []]
       ,Tree "B" [Tree ("  r") [],Tree ("  s") []]
       ,Tree "C" []]

The function `HS.level` returns the number of leading spaces divided by 2 --
integer division, no remainder.

-}
fromList : a -> (a -> Int) -> List a -> Tree a
fromList rooLabel level lst =
    lst
        |> List.foldl (\s z -> step level s z) (Zipper.fromTree (Tree.singleton rooLabel))
        |> Zipper.toTree


step : (a -> Int) -> a -> Zipper a -> Zipper a
step level s z =
    let
        ld =
            levelDifference level s z
    in
    case ld of
        Nothing ->
            appendAtFocus level s z

        Just 0 ->
            appendAtFocus level s z

        Just 1 ->
            addChildAtFocus level s z

        _ ->
            let
                levelsBack =
                    negate (ld |> Maybe.withDefault 0)
            in
            addAtNthParent level levelsBack s z


{-| The `tagWithDepth` function transforms a tree of items into
a tree of tuples of the form `(a, k)`, where `k` is the
depth of `a` in the tree.

    > tagWithDepth (Tree.map String.trim t)
      Tree ("*",0) [
        Tree ("A",1) [Tree ("p",2) [],Tree ("q",2) []]
       ,Tree ("B",1) [Tree ("r",2) [],Tree ("s",2) []]
       ,Tree ("C",1) []]

-}
tagWithDepth : Tree a -> Tree ( a, Int )
tagWithDepth t =
    tagWithDepth_ ( t, 0 )


tagWithDepth_ : ( Tree a, Int ) -> Tree ( a, Int )
tagWithDepth_ ( t, k ) =
    let
        c =
            Tree.children t
    in
    Tree.tree ( Tree.label t, k ) (List.map (\t_ -> tagWithDepth_ ( t_, k + 1 )) c)


{-| Map a tree to a list of node contents:

    > import Tree

    > t |> Tree.map (\label -> String.trim label)
      Tree "*" [
         Tree "A" [Tree "p" [],Tree "q" []]
        ,Tree "B" [Tree "r" [],Tree "s" []]
        ,Tree "C" []]

     > t |> Tree.map (\label -> String.trim label) |> lffla
       ["*","A","p","q","B","r","s","C"]

-}
flatten : Tree b -> List b
flatten t =
    Tree.flatten t



-- ADDING THINGS --


appendAtFocus : (a -> Int) -> a -> Zipper a -> Zipper a
appendAtFocus level s z =
    let
        t =
            Zipper.tree z

        newTree =
            Tree.appendChild (singleton s) t
    in
    Zipper.replaceTree newTree z


addChildAtFocus : (a -> Int) -> a -> Zipper a -> Zipper a
addChildAtFocus level s z =
    case Zipper.lastChild z of
        Nothing ->
            z

        Just zz ->
            appendAtFocus level s zz


addChildAtParentOfFocus : (a -> Int) -> a -> Zipper a -> Zipper a
addChildAtParentOfFocus level s z =
    case Zipper.parent z of
        Nothing ->
            z

        Just zz ->
            appendAtFocus level s zz


addAtNthParent : (a -> Int) -> Int -> a -> Zipper a -> Zipper a
addAtNthParent level k s z =
    case manyParent k z of
        Nothing ->
            z

        Just zz ->
            appendAtFocus level s zz


nthParentOfFocus : Int -> Zipper a -> Zipper a
nthParentOfFocus k z =
    case manyParent k z of
        Nothing ->
            z

        Just zz ->
            zz



-- MOVING AROUND --


manyParent : Int -> Zipper a -> Maybe (Zipper a)
manyParent k z =
    let
        zz =
            Zipper.parent z
    in
    iterate (k - 1) (\zi -> Maybe.andThen Zipper.parent zi) zz


manyBackward : Int -> Zipper a -> Maybe (Zipper a)
manyBackward k z =
    if k < 0 then
        Nothing

    else if k == 0 then
        Just z

    else
        let
            zz =
                Zipper.backward z
        in
        iterate k (\zi -> Maybe.andThen Zipper.backward zi) zz


iterate : Int -> (a -> a) -> a -> a
iterate k f x =
    List.foldl (\i acc -> f acc) x (List.range 1 k)



-- LEVELS --


levelOfLastChild : (a -> Int) -> Zipper a -> Maybe Int
levelOfLastChild level z =
    Zipper.lastChild z
        |> Maybe.map Zipper.tree
        |> Maybe.map Tree.label
        |> Maybe.map level


levelDifference : (a -> Int) -> a -> Zipper a -> Maybe Int
levelDifference level s z =
    Maybe.map2 (-) (Just <| level s) (levelOfLastChild level z)



-- STANDARD TREE FUNCTIONS --


{-| depth t is the depth of the tree.

    > depth t
    2 : Int

-}
depth : Tree a -> Int
depth t =
    let
        c =
            Tree.children t
    in
    if c == [] then
        0

    else
        1 + listMax (List.map depth c)


{-| nodecount t is the number of notes in the tree t

    > nodeCount t
    8 : Int

The count includes the root.

-}
nodeCount : Tree a -> Int
nodeCount t =
    let
        c =
            Tree.children t
    in
    if c == [] then
        1

    else
        1 + List.sum (List.map nodeCount c)


listMax : List Int -> Int
listMax ints =
    List.foldl (\i acc -> max i acc) 0 ints



-- STRING REPRESENTATION --


{-| Given a function that maps labels to strings, construct
a string that represents the tree as an outline.

    > t |> toOutline identity
    "*\nA\n    p\n    q\nB\n    r\n    s\nC" : String

The string returned is

    *
        A
            p
            q
        B
            r
            s
        C

-}
toOutline : (a -> String) -> Tree a -> String
toOutline stringOfLabel t =
    t
        |> tagWithDepth
        |> toOutline_ stringOfLabel


toOutline_ : (a -> String) -> Tree ( a, Int ) -> String
toOutline_ stringOfLabel t =
    let
        stringOfLabel_ t_ =
            Tree.label t_ |> Tuple.first |> stringOfLabel

        c =
            Tree.children t

        ( label_, level_ ) =
            Tree.label t |> (\( la, le ) -> ( stringOfLabel_ t, le ))

        n =
            2 * (level_ - 1)

        prefix =
            String.repeat n " "

        lab =
            prefix ++ label_
    in
    if c == [] then
        lab

    else
        lab ++ "\n" ++ (List.map (toOutline_ stringOfLabel) c |> String.join "\n")
