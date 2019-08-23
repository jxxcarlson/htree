module HTree exposing (fromList, flatten, toOutline, tagWithDepth, depth, nodeCount)


{-| Working with hierarchical lists.
  - convert a hierarchical list to a rose tree
  - convert a rose tree to a list of node labels
  - convert a rose tree to a string representing the corresponding outline
  - etc.
@docs fromList, flatten, toOutline, depth, nodeCount, tagWithDepth
-}

import Tree exposing (Tree, singleton)
import Tree.Zipper as Zipper exposing (Zipper)

{-| Given:
  - a root element of type a
  - a function that determines the level of an item
  - a list of items of type a

`fromList` returns the corresponding rose tree. For an example this snippet

    import Example.Test as Example exposing (o2)
    import HTree.String as HS
    import Tree exposing (Tree)

    data : List String
    data =
        String.split "\n" o2
            |> List.filter (not << String.isEmpty)
    data
        --> ["A","  p","  q","B","  r","  s","C"]

    tree : Tree String
    tree =
        -- `HS.level` returns the number of leading spaces
        -- divided by 2 using integer division.
        fromList "*" HS.level data

        --> Tree "*"
        --> [ Tree "A" [ Tree "  p" [], Tree "  q" [] ]
        --> , Tree "B" [ Tree "  r" [], Tree "  s" [] ]
        --> , Tree "C" []
        --> ]

produces the outline

    A
        p
        q
    B
        r
        s
    C
-}
fromList : a -> (a -> Int) -> List a -> Tree a
fromList rootLabel level lst =
    lst
        |> List.foldl (\s z -> step level s z) (Zipper.fromTree (Tree.singleton rootLabel))
        |> Zipper.toTree


step : (a -> Int) -> a -> Zipper a -> Zipper a
step level s z =
    let
        ld =
            levelDifference level s z
    in
    case ld of
        Nothing ->
            appendAtFocus s z

        Just 0 ->
            appendAtFocus s z

        Just 1 ->
            addChildAtFocus s z

        _ ->
            let
                levelsBack =
                    negate (ld |> Maybe.withDefault 0)
            in
            addAtNthParent levelsBack s z


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


appendAtFocus : a -> Zipper a -> Zipper a
appendAtFocus s z =
    let
        t =
            Zipper.tree z

        newTree =
            Tree.appendChild (singleton s) t
    in
    Zipper.replaceTree newTree z


addChildAtFocus : a -> Zipper a -> Zipper a
addChildAtFocus s z =
    case Zipper.lastChild z of
        Nothing ->
            z

        Just zz ->
            appendAtFocus s zz


addAtNthParent : Int -> a -> Zipper a -> Zipper a
addAtNthParent  k s z =
    case manyParent k z of
        Nothing ->
            z

        Just zz ->
            appendAtFocus s zz


-- MOVING AROUND --


manyParent : Int -> Zipper a -> Maybe (Zipper a)
manyParent k z =
    let
        zz =
            Zipper.parent z
    in
    iterate (k - 1) (\zi -> Maybe.andThen Zipper.parent zi) zz


iterate : Int -> (a -> a) -> a -> a
iterate k f x =
    List.foldl (\_ acc -> f acc) x (List.range 1 k)



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
        |> toOutlineHelp stringOfLabel

{-| This version of `outLineHelp` using Ilias' `restructure`
is courtesy of Folkert de Vries.  Thanks Folkert!
Much nicer!

-}
toOutlineHelp labelToString =
    let
        combine : ( String, Int ) -> List String -> String
        combine ( currentLabel, currentLevel ) children =
            let
                prefix =
                    String.repeat (2 * (currentLevel - 1)) " "
            in
            case children of
                [] ->
                    prefix ++ currentLabel

                _ ->
                    prefix ++ currentLabel ++ "\n" ++ String.join "\n" children
    in
    Tree.restructure (Tuple.mapFirst labelToString) combine