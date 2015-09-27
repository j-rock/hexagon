------------------------------------------------------------
-- |
-- Copyright    : (C) 2015 Joseph Rock
-- License      : MIT (see the file LICENSE)
--
-- Maintainer   : Joseph Rock <wax818@gmail.com>
-- Portability  : non-portable
------------------------------------------------------------

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}

module Data.Hexagon
    (
      Hexagon

      -- * Grid orientations
    , FlatTop(..)
    , PointyTop(..)

      -- * Relative directions
    , TopBottomDir(..)
    , EastWestDir(..)
    , Orientation(..)
    , neighbors

      -- * Inter-hexagon operations
    , binOp
    , plus
    , minus
    , times

      -- * Adjacency
    , dist

      -- * Building collections
    , lineTo
    , range
    , rangeWithBlocked
    , ringAround

      -- * Rotations
    , RotateDir(..)
    , rotateAboutOrigin
    , rotateAroundPoint

      -- * Alternative coordinate systems
    , HexCoordTuple(..)
    , OddR(..)
    , EvenR(..)
    , OddQ(..)
    , EvenQ(..)
    , Axial(..)
    )  where

import           Control.Monad            (forM_, replicateM_, unless)
import           Control.Monad.ST         (runST)
import qualified Data.Bits                as Bits
import           Data.Hashable            (Hashable)
import qualified Data.HashTable.Class     as HT
import qualified Data.HashTable.ST.Cuckoo as HTC
import           Data.Maybe               (isJust)
import qualified Data.STRef               as ST
import           GHC.Generics             (Generic)
import           Prelude                  hiding (Left, Right)


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
neighbors hex = map (plus hex) [fromXY x y | x <- [-1, 0, 1], y <- [-1, 0, 1], x /= y]

-- | The fundamental coordinate type for a 2D hexagonal grid.
data Hexagon ori = Hex Integer Integer Integer
                   deriving (Eq, Ord, Show, Generic)

instance Hashable (Hexagon ori)

fromXY :: Integer -> Integer -> Hexagon ori
fromXY x y = Hex x y (-x-y)

fromXZ :: Integer -> Integer -> Hexagon ori
fromXZ x z = Hex x (-x-z) z

-- | The Odd-R Offset coordinate system. See http://www.redblobgames.com/grids/hexagons/
data OddR = OddR
-- | The Even-R Offset coordinate system. See http://www.redblobgames.com/grids/hexagons/
data EvenR = EvenR
-- | The Odd-Q Offset coordinate system. See http://www.redblobgames.com/grids/hexagons/
data OddQ = OddQ
-- | The Even-Q Offset coordinate system. See http://www.redblobgames.com/grids/hexagons/
data EvenQ = EvenQ
-- | The Axial Offset coordinate system. See http://www.redblobgames.com/grids/hexagons/
data Axial = Axial

i2dConv outer inner ind dep =
  let cleanOffset = dep `inner` (dep Bits..&. 1)
  in ind `outer` Bits.shift cleanOffset 1

-- | A class to structure conversions between coordinate systems
class HexCoordTuple t where
  toHexagon   :: t -> (Integer, Integer) -> Hexagon ori
  fromHexagon :: t -> Hexagon ori -> (Integer, Integer)

instance HexCoordTuple OddR where
  toHexagon   _ (q,r)       = fromXZ (i2dConv (-) (-) q r) r
  fromHexagon _ (Hex x _ z) = (i2dConv (+) (-) x z, z)

instance HexCoordTuple EvenR where
  toHexagon   _ (q,r)       = fromXZ (i2dConv (-) (+) q r) r
  fromHexagon _ (Hex x _ z) = (i2dConv (+) (+) x z, z)

instance HexCoordTuple OddQ where
  toHexagon   _ (q,r)       = fromXZ q $ i2dConv (-) (-) r q
  fromHexagon _ (Hex x _ z) = (x, i2dConv (+) (-) z x)

instance HexCoordTuple EvenQ where
  toHexagon   _ (q,r)       = fromXZ q $ i2dConv (-) (+) r q
  fromHexagon _ (Hex x _ z) = (x, i2dConv (+) (+) z x)

instance HexCoordTuple Axial where
  toHexagon   _             = uncurry fromXZ
  fromHexagon _ (Hex x _ z) = (x,z)

-- | Kind of like a monomorphic zipWith on two Hexagons.
-- In pseudo-Haskell:
--
-- @
-- let ax = toHexagon Axial
--     h  = (x, y ) :: (Integer, Integer)
--     h' = (x',y') :: (Integer, Integer)
--     f            :: Integer -> Integer -> Integer
-- binOp f (ax h) (ax h') \<-\> ax $ (f x x', f y y')
-- @
binOp :: (Integer -> Integer -> Integer) -> Hexagon ori -> Hexagon ori -> Hexagon ori
binOp f (Hex x y z) (Hex x' y' z') = Hex (f x x') (f y y') (f z z')

-- | Adds the corresponding components of two Hexagons.
--
-- @
-- plus = binOp (+)
-- @
plus :: Hexagon ori -> Hexagon ori -> Hexagon ori
plus = binOp (+)

-- | Subtracts the corresponding components of two Hexagons.
--
-- @
-- minus = binOp (-)
-- @
minus :: Hexagon ori -> Hexagon ori -> Hexagon ori
minus = binOp (-)

-- | Multiplies the corresponding components of two Hexagons.
--
-- @
-- times = binOp (*)
-- @
times :: Hexagon ori -> Hexagon ori -> Hexagon ori
times = binOp (*)

-- | Computes the distance between two Hexagons.
-- This distance can be thought of the length of the straight-line
-- path between two Hexagons.
dist :: Hexagon ori -> Hexagon ori -> Integer
dist (Hex x y z) (Hex x' y' z') = maximum . map abs $ [x-x', y-y', z-z']

hlerp :: Hexagon ori -> Hexagon ori -> Float -> (Float, Float, Float)
hlerp (Hex x y z) (Hex x1 y1 z1) t =
    let combine :: Integer -> Integer -> Float
        combine p p1 = t * fromInteger (p1 - p) + fromInteger p
    in (combine x x1, combine y y1, combine z z1)

-- | Returns a list of all Hexagons on the straight-line path from the
-- first Hexagon to the second Hexagon, including both as endpoints.
lineTo :: Hexagon ori -> Hexagon ori -> [Hexagon ori]
lineTo a b| a == b = [a]
          | otherwise = let d      = dist a b
                            frac i = (fromInteger i / fromInteger d :: Float)
                        in map (roundToHex . hlerp a b . frac) [0..d]

roundToHex :: (Float, Float, Float) -> Hexagon ori
roundToHex (x,y,z) =
    let getRoundErr :: Float -> (Integer, Float)
        getRoundErr v = (rounded, abs $ v - fromInteger rounded)
            where rounded = round v

        (rx, dx) = getRoundErr x
        (ry, dy) = getRoundErr y
        (rz, dz) = getRoundErr z

        (x', y', z')| dx > dy && dx > dz = (negate $ ry + rz, ry, rz)
                    | dy > dz            = (rx, negate $ rx + rz, rz)
                    | otherwise          = (rx, ry, negate $ rx + ry)
    in Hex x' y' z'

type HexagonalRange = (Integer, Integer)

generalizedRangeCollect :: HexagonalRange -- (xmin, xmax)
                        -> HexagonalRange -- (ymin, ymax)
                        -> HexagonalRange -- (zmin, zmax)
                        -> [Hexagon ori]
generalizedRangeCollect (xmin, xmax) (ymin,ymax) (zmin,zmax) =
    do x <- [xmin .. xmax]
       y <- [max ymin (negate $ x+zmax) .. min ymax (negate $ x+zmin)]
       let z = negate (x + y)
       return (Hex x y z)

-- | All Hexagons within a given distance from the argument Hexagon.
range :: Hexagon ori -> Integer -> [Hexagon ori]
range (Hex x y z) n = generalizedRangeCollect xs ys zs
    where xs = (x - n, x + n)
          ys = (y - n, y + n)
          zs = (z - n, z + n)

{-| Same as 'range' except it factors in which Hexagons are blocked.

The predicate you pass to this function asks if a given Hexagon is blocked. For example:

@
let origin                     = toHexagon Axial (0,0)
    blockCircleWithRadius4 hex = dist origin hex < 4
    take10Steps                = 10
    startHex                   = toHexagon Axial (2,2)
in rangeWithBlocked startHex take10Steps blockCircleWithRadius4
@

In the above example, the function starts at the Hexagon located at Axial(2,2).
It takes as many as ten steps from that start location. It skips any Hexagon that
is closer than four units from the origin. The result it returns is all of the
Hexagons that could have been stepped on.
-}
rangeWithBlocked :: Hexagon ori -> Integer -> (Hexagon ori -> Bool) -> [Hexagon ori]
rangeWithBlocked start n isBlocked =
    runST $ do visited <- newSet
               unless (isBlocked start) (visit visited start)
               oldFringeRef <- ST.newSTRef [start]
               replicateM_ (fromInteger n) $ do
                  (oldFringe, newFringeRef) <- fringeInit oldFringeRef
                  forM_ oldFringe $ \hexagon ->
                      forM_ (neighbors hexagon) $ \neigh -> do
                          seenAlready <- hasBeenVisited visited neigh
                          unless (seenAlready || isBlocked neigh) $ do
                              visit visited neigh
                              addToFringe newFringeRef neigh
                  assign oldFringeRef newFringeRef
               getVisited visited
        where newSet = HTC.new
              fringeInit ofr = (,) <$> ST.readSTRef ofr <*> ST.newSTRef []
              hasBeenVisited v h = isJust <$> HT.lookup v h
              visit vs h = HT.insert vs h ()
              addToFringe fr h = ST.modifySTRef fr ((:) h)
              assign ofr nfr = do new <- ST.readSTRef nfr
                                  ST.writeSTRef ofr new
              getVisited vs = map fst <$> HT.toList vs

-- | Simple sum type for Clockwise/Counter-clockwise.
data RotateDir = CW | CCW deriving (Eq, Ord, Show, Enum, Bounded)

-- | Rotate a Hexagon 60 degrees in a given direction about the origin.
rotateAboutOrigin :: RotateDir -> Hexagon ori -> Hexagon ori
rotateAboutOrigin CW  (Hex x y z) = Hex (-z) (-x) (-y)
rotateAboutOrigin CCW (Hex x y z) = Hex (-y) (-z) (-x)

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

-- | Retrieves all Hexagons contained in the ring of given radius about the
-- argument Hexagon.
--
-- In pseudo-Haskell:
--
-- @
-- ringAround h i = [h' | h' <- allHexagons, dist h h' == i]
-- @
ringAround :: Hexagon ori -> Integer -> [Hexagon ori]
ringAround center 0      = [center]
ringAround center radius = ns >>= ringEdge
  where   -- Order is important in this list
          ns = [ Hex 1 (-1) 0
               , Hex 0 (-1) 1
               , Hex (-1) 0 1
               , Hex (-1) 1 0
               , Hex 0 1 (-1)
               , Hex 1 0 (-1)
               ]

          ringEdgeVector (Hex x y z) = Hex z x y

          ringEdge re@(Hex x y z) =
              let start = center `plus` Hex (radius*x) (radius*y) (radius*z)
              in take (fromInteger radius) . iterate (`plus` ringEdgeVector re) $ start
