module Example.Test exposing (assignments, o1, o2, o3, o4, o4w, o4y, o4z, root)


o1 : String
o1 =
    """A
B
C
"""

o2 : String
o2 =
    """A
  p
  q
B
"""

o2b : String
o2b =
    """A
  p
  q
B
  r
  s
C
"""



o3 : String
o3 =
    """A
  p
    1
    2
    3
  q
B
  r
  s
C
"""


o4 : String
o4 =
    """A
  p
    1
    2
    3
      alpha
      beta
  q
B
  r
  s
C
"""


o4y : String
o4y =
    """A
  p
    1
    2
    3
      alpha
      beta
    y
"""


o4z : String
o4z =
    """A
  p
    1
    2
    3
      alpha
      beta
  z
"""


o4w : String
o4w =
    """A
  p
    1
    2
    3
      alpha
      beta
w
"""


assignments : List ( Int, String )
assignments =
    [ ( 1, "Groceries" )
    , ( 2, "Fred" )
    , ( 2, "Mary" )
    , ( 1, "Cooks" )
    , ( 2, "Sarah" )
    , ( 2, "John" )
    , ( 1, "Cleanup crew" )
    , ( 2, "George" )
    , ( 2, "Susan" )
    ]


root : ( Int, String )
root =
    ( 0, "Assignments" )
