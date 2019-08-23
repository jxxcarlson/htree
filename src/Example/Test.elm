module Example.Test exposing (o1, o2, o3, o4, o4y, o4z, o4w)

o1: String
o1 =
    """A
B
C
"""

o2: String
o2 =
    """A
  p
  q
B
  r
  s
C
"""

o3: String
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

o4: String
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

o4y: String
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

o4z: String
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

o4w: String
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

