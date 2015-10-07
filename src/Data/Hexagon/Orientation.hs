{-# LANGUAGE TypeFamilies #-}

module Data.Hexagon.Orientation where

import           Data.Hexagon.Hexagon

{-|
The orientation in which hexagons have a horizontal edge on the top.

@
Pictorially:      __
               \_\_/  \\__
              \/  \\__\/  \\
              \\_\_\/  \\_\_\/
              \/  \\_\_\/  \\
              \\_\_\/  \\_\_\/
                 \\_\_\/
@
-}
data FlatTop = FlatTop deriving (Eq, Ord, Show, Enum, Bounded)

{-|
The orientation in which hexagons have vertical edges on the sides.

@
Pictorially:
               \/\\   \/\\
              \/  \\ \/  \\
             |    |    |
             |    |    |
              \\  \/ \\  \/
               \\\/   \\\/
@
-}
data PointyTop = PointyTop deriving (Eq, Ord, Show, Enum, Bounded)


-- | Locks down which relative directions correspond to edges/corners on
-- a given orientation.
class Orientation ori where
    -- | Relative directions of edges on a given hexagon.
    type EdgeDir   ori :: *
    -- | Relative directions of corners on a given hexagon.
    type CornerDir ori :: *
    -- | Compute the hexagon which borders the argument hexagon on the
    -- relative edge.
    neighbor    :: EdgeDir ori -> Hexagon ori -> Hexagon ori

instance Orientation FlatTop where
    type EdgeDir   FlatTop = TopBottomDir
    type CornerDir FlatTop = EastWestDir

    neighbor BottomRight = plus $ Hex 1 (-1) 0
    neighbor Bottom      = plus $ Hex 0 (-1) 1
    neighbor BottomLeft  = plus $ Hex (-1) 0 1
    neighbor TopLeft     = plus $ Hex (-1) 1 0
    neighbor Top         = plus $ Hex 0 1 (-1)
    neighbor TopRight    = plus $ Hex 1 0 (-1)

instance Orientation PointyTop where
    type EdgeDir   PointyTop = EastWestDir
    type CornerDir PointyTop = TopBottomDir
    neighbor East       = plus $ Hex 1 (-1) 0
    neighbor SouthEast  = plus $ Hex 0 (-1) 1
    neighbor SouthWest  = plus $ Hex (-1) 0 1
    neighbor West       = plus $ Hex (-1) 1 0
    neighbor NorthWest  = plus $ Hex 0 1 (-1)
    neighbor NorthEast  = plus $ Hex 1 0 (-1)

-- | Retrieves all neighbors for a hexagon in no particular order.
neighbors :: Hexagon ori -> [Hexagon ori]
neighbors hex = map (plus hex) [Hex x (-x-z) z | x <- [-1, 0, 1], z <- [-1, 0, 1], x /= z]

{-|
Relative directions with respect to \"Top-Bottom\"-ness.

On a FlatTop, these correspond to the edges:

@

            Top
            ____
   TopLeft /    \\ TopRight
          /      \\
          \\      /
BottomLeft \\____/ BottomRight
           Bottom
@

On a PointyTop, these correspond to the corners:

@
           Top
            /\\
  TopLeft  /  \\ TopRight
          |    |
          |    |
BottomLeft \\  / BottomRight
            \\/
          Bottom
@
-}
data TopBottomDir = BottomRight
                  | Bottom
                  | BottomLeft
                  | TopLeft
                  | Top
                  | TopRight
                    deriving (Eq, Ord, Show, Enum, Bounded)

{-|
Relative directions with respect to \"East-West\"-ness.

On a FlatTop, these correspond to the corners:

@
NorthWest ____ NorthEast
         /    \\
   West /      \\  East
        \\      /
         \\____/
 SouthWest    SouthEast
@

On a PointyTop, these correspond to the edges:

@

NorthWest /\\ NorthEast
         /  \\
        |    |
   West |    | East
         \\  /
SouthWest \\/ SouthEast
@
-}
data EastWestDir = East
                 | SouthEast
                 | SouthWest
                 | West
                 | NorthWest
                 | NorthEast
                   deriving (Eq, Ord, Show, Enum, Bounded)

-- | Simple sum type for Clockwise/Counter-clockwise.
data RotateDir = CW | CCW deriving (Eq, Ord, Show, Enum, Bounded)

{-|
Rotate a Hexagon 60 degrees in a given direction about a center Hexagon.

@
let rotationCenter :: Hexagon ori
    pointToRotate  :: Hexagon ori
in rotateAroundPoint rotationCenter CCW pointToRotate
@
-}
rotateAroundPoint :: Hexagon ori -> RotateDir -> Hexagon ori -> Hexagon ori
rotateAroundPoint center rdir h = center `plus` rotateAboutOrigin rdir (h `minus` center)

{-| Rotate a Hexagon 60 degrees in a given direction about the origin.

@
rotateAboutOrigin \<-\> rotateAroundPoint (toHexagon Axial (0,0))
@
-}
rotateAboutOrigin :: RotateDir -> Hexagon ori -> Hexagon ori
rotateAboutOrigin CW  (Hex x y z) = Hex (-z) (-x) (-y)
rotateAboutOrigin CCW (Hex x y z) = Hex (-y) (-z) (-x)
