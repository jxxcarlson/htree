module Example.Test exposing (o1, o2, o3, o3a, o3b, o4, o4a, o4w, o4x, o4y, o4z)


o1 =
    """A
B
C
"""


o2 =
    """A
  p
  q
B
  r
  s
C
"""


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


o3a =
    """A
  p
    1
    2
    3
"""


o3b =
    """A
  p
    1
    2
    3
  q
"""


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


o4x =
    """A
  p
    1
    2
    3
      alpha
      beta
"""


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


o4a =
    """A
  p
    1
    2
    3
      alpha
      beta
    X
  q
B
  r
  s
C
"""
