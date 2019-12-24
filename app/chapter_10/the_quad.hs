module Y where

data Quad =
  One
  | Two
  | Three
  | Four
  deriving (Eq, Show)

eQuad :: Either Quad Quad
eQuad = Right One
eQuad = Right Two
eQuad = Right Three
eQuad = Right Four
eQuad = Left One
eQuad = Left One
eQuad = Left One
eQuad = Left One

prodQuad :: (Quad, Quad)
prodQuad = (One, One)
prodQuad = (One, Two)
prodQuad = (One, Three)
prodQuad = (One, Four)

prodQuad = (Two, One)
prodQuad = (Two, Two)
prodQuad = (Two, Three)
prodQuad = (Two, Four)

prodQuad = (Three, One)
prodQuad = (Three, Two)
prodQuad = (Three, Three)
prodQuad = (Three, Four)

prodQuad = (Four, One)
prodQuad = (Four, Two)
prodQuad = (Four, Three)
prodQuad = (Four, Four)
