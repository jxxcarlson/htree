module HTree exposing (depth, flatten, fromList, nodeCount, tagWithDepth, toOutline)

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

`fromList` returns the corresponding rose tree. For an example, consider this snippet

    -- IMPORTS
    import Example.Test as Example exposing (o2)
    import HTree.String as HS
    import Tree exposing (Tree)
    import HTree exposing(..)

    -- INPUT STRING
    o2 =
    """
    A
      p
      q
    B
    """

    -- CODE
    data : List String
    data = String.split "\n" o2 |> List.filter (not << String.isEmpty)
        --> ["A","  p","  q","B"]

    tree : Tree String
    tree =
        -- `HS.level` returns the number of leading spaces
        -- divided by 2 using integer division.
        fromList "*" HS.level data

        --> Tree "*"
        --> [ Tree "A" [ Tree "  p" [], Tree "  q" [] ]
        --> , Tree "B" []
        --> ]
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

    > tagWithDepth (Tree.map String.trim tree)
      Tree ("*",0) [
        Tree ("A",1) [Tree ("p",2) [],Tree ("q",2) []]
       ,Tree ("B",1) []
      ]

-}
tagWithDepth : Tree a -> Tree ( a, Int )
tagWithDepth t =
    tagWithDepthHelp 0 t


tagWithDepthHelp : Int -> Tree a -> Tree ( a, Int )
tagWithDepthHelp k t =
    let
        c =
            Tree.children t
    in
    Tree.tree ( Tree.label t, k ) (List.map (\t_ -> tagWithDepthHelp (k + 1) t_) c)


{-| Flatten a tree into a list of labels.

    Tree.map (\label -> String.trim label) tree
        --> Tree "*"
        -->     [ Tree "A" [ Tree "p" [], Tree "q" [] ]
        -->     , Tree "B" []
        -->     ]

    Tree.map (\label -> String.trim label) tree
        |> toList
            --> [ "*", "A", "p", "q", "B"]

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
    Zipper.lastChild z
        |> Maybe.map (appendAtFocus s)
        |> Maybe.withDefault z


addAtNthParent : Int -> a -> Zipper a -> Zipper a
addAtNthParent k s z =
    manyParent k z
        |> Maybe.map (appendAtFocus s)
        |> Maybe.withDefault z



-- MOVING AROUND --


manyParent : Int -> Zipper a -> Maybe (Zipper a)
manyParent k z =
    let
        zz =
            Zipper.parent z
    in
    iterate (k - 1) (\zi -> Maybe.andThen Zipper.parent zi) zz


iterate : Int -> (a -> a) -> a -> a
iterate remaining f accumulator =
    if remaining > 0 then
        iterate (remaining - 1) f (f accumulator)

    else
        accumulator



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
-}
toOutline : (a -> String) -> Tree a -> String
toOutline stringOfLabel t =
    t
        |> tagWithDepth
        |> toOutlineHelp stringOfLabel

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
