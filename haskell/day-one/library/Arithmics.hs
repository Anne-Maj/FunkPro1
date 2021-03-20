module Arithmics where

data Vector3D = Vector3D
  { x :: Float
  , y :: Float
  , z :: Float
  } deriving (Show)

add3D :: Vector3D -> Vector3D -> Vector3D
add3D v1 v2 =
  Vector3D (x v1 + x v2) (y v1 + y v2) (z v1 + z v2)
{-
add3D (Vector3D x1 y1 z1) (Vector3D x2 y2 z2) =
  Vector3D (x1 + x2) (y1 + y2) (z1 + z2)
-}

-- not used here ;-)
-- sumOfList :: [Int] -> Int
-- sumOfList [] = 0
-- sumOfList first : rest = first + sumOfList rest

{- Elm equivalent
sumOfList : List Int -> Int
sumOfList l =
  case l of
    [] -> 0
    first :: rest -> first + sumOfList rest
-}

sub3D :: Vector3D -> Vector3D -> Vector3D
sub3D (Vector3D x1 y1 z1) (Vector3D x2 y2 z2) =
  Vector3D (x1 - x2) (y1 - y2) (z1 - z2)

mul3D :: Vector3D -> Float -> Vector3D
mul3D (Vector3D x1 y1 z1) f =
  Vector3D (f*x1) (f*y1) (f*z1)

dot3D :: Vector3D -> Vector3D -> Float
dot3D (Vector3D x1 y1 z1) (Vector3D x2 y2 z2) =
  x1*x2 + y1*y2 + z1*z2

size3D :: Vector3D -> Float
size3D v = sqrt (dot3D v v)
-- size3D (Vector3D x' y' z') = sqrt (x'**2 + y'*y' + z'**2)

data Line3D = Line3D
  { origo :: Vector3D
  , unit :: Vector3D
  } deriving (Show)

data Sphere3D = Sphere3D
  { center :: Vector3D
  , radius :: Float
  } deriving (Show)

eye :: Vector3D
eye = Vector3D 0 0 0

sphere :: Sphere3D
sphere = Sphere3D (Vector3D 0 10 0) 2

lineFrom :: Vector3D -> Vector3D -> Line3D
lineFrom o t = Line3D o u where
  v = sub3D t o -- find vector from o to t : t - o
  u = mul3D v (1.0/size3D v)

dist3D :: Sphere3D -> Line3D -> Maybe Float
dist3D (Sphere3D c r) (Line3D o u) =
  if f < 0 then Nothing
  else Just ((-b - sqrt f)/2.0)
  where
    oc = sub3D o c
    b = 2*(dot3D u oc)
    f = b**2 - 4*(dot3D oc oc - r**2)

linePoint3D :: Line3D -> Float -> Vector3D
linePoint3D (Line3D o u) distance =
  add3D o (mul3D u distance)


reflect3D :: Sphere3D -> Line3D -> Maybe Line3D
reflect3D sphere line = fmap reflected distance where
  distance = dist3D sphere line
  reflected :: Float -> Line3D
  reflected d = Line3D t v where
    t = linePoint3D line d
    u = unit line
    n = mul3D (sub3D t (center sphere)) (1.0/(radius sphere))
    v = add3D u (mul3D n (-2*(dot3D u n)))

reflectedLine3D :: Sphere3D -> Line3D -> Line3D
reflectedLine3D sphere line =
  case (reflect3D sphere line) of
    Just reflection -> reflection
    Nothing -> line

data Color3D = SkyColor | DarkColor | BrightColor deriving (Show)

-- xi = hvor langt til siden på jorden
-- yi = hvor langt ude på jorden

reflectedColor3D :: Line3D -> Color3D
reflectedColor3D (Line3D o u)
  | (z u) >= 0 =                                 SkyColor
  | (mod (round xi) 3)*(mod (round yi) 3) == 0 = DarkColor
  | otherwise =                                  BrightColor
  where
    h = (z o) - 2
    f = -h/(z u)
    xi = (x o) + (x u)*f
    yi = (y o) + (y u)*f
