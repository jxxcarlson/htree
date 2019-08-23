
# HTree

The purpose of this module is to transform hierarchical lists
into trees, where trees are as in `zwilias/elm-rosetree`.  A hierarchical
list is by definition a list of items
endowed with a function

```
   level : Item -> Int
```

and a root label, e.g., `"*"` in the case of lists 
of strings.

The `level` function, which associates non-negative
integers to items, defines the hierarchy. As an example,
consider the course outline below

```
Introduction
Data Structures
  Lists
  Trees
  Queues
Algorithms
  Brute-force-search
  Binary search
  Probabalistic methods
Compational complexity
  O(n) notation
  Polynomial time 
  Exponential time
...
```

If we take the root label to be *Book* and the level of an
item to be the number of leading spaces divided by two, then this outline
is a hierarchical list and so defines a tree.  The childreen of *Book* are are *Introduction*, 
*Data Structures*, etc. *Introduction* has no children,
while *Data Structures* has three.  Etc.

One can turn the above outline into a tree like this:

```
> import HTree.String

> ourtline = """
Introduction
Data Structures
  Lists
  Trees
  ...
"""

> t = HTree.String.parse ourtline
  Tree "*" 
     [ Tree "Introduction" []
      ,Tree ("Data Structures") [Tree "Lists" [],Tree "Trees" [],Tree "Queues" []]
      ,Tree "Algorithms" [Tree "Brute-force-search" []
      ,Tree ("Binary search") [],Tree ("Probabalistic methods") []]
      ,Tree ("Compational complexity") [Tree ("O(n) notation") []
      ,Tree ("Polynomial time") [],Tree ("Exponential time") []],Tree "" []]
```

Conversely, a tree can be turned into an outline:

```
> HTree.String.toOutline t 
  ...
```
```