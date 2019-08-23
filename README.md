
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
  O(n) nottation
  Polynomial time 
  Exponential time
...
```

If we take the root label to be **Book**, then this outline
defines a tree.  The childreen of **Book** are are **Introduction**, 
**Data Structures**, etc. **Introduction** has no children,
while **Data Structures** has three.  Etc.