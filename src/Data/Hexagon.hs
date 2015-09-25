{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Data.Hexagon where

import           Control.Monad            (forM_, replicateM_, unless)
import           Control.Monad.ST         (runST)
import           Data.Hashable            (Hashable)
import qualified Data.HashTable.Class     as HT
import qualified Data.HashTable.ST.Cuckoo as HTC
import           Data.Maybe               (isJust)
import qualified Data.STRef               as ST
import           GHC.Generics             (Generic)
import           Prelude                  hiding (Left, Right)

data FlatTop = FlatTop deriving (Eq, Ord, Show, Enum, Bounded)
data PointyTop = PointyTop deriving (Eq, Ord, Show, Enum, Bounded)

data AboveBelowDir = BelowRight
                   | Below
                   | BelowLeft
                   | AboveLeft
                   | Above
                   | AboveRight
                     deriving (Eq, Ord, Show, Enum, Bounded)

data LeftRightDir = Right
                  | RightBelow
                  | LeftBelow
                  | Left
                  | LeftAbove
                  | RightAbove
                    deriving (Eq, Ord, Show, Enum, Bounded)

type family CornerDirection ori where
    CornerDirection FlatTop   = LeftRightDir
    CornerDirection PointyTop = AboveBelowDir

type family EdgeDirection ori where
    EdgeDirection FlatTop   = AboveBelowDir
    EdgeDirection PointyTop = LeftRightDir

class Orientation ori where
    neighbor    :: EdgeDirection ori -> Hexagon ori -> Hexagon ori

instance Orientation FlatTop where
    neighbor BelowRight = hplus $ Hex 1 (-1) 0
    neighbor Below      = hplus $ Hex 0 (-1) 1
    neighbor BelowLeft  = hplus $ Hex (-1) 0 1
    neighbor AboveLeft  = hplus $ Hex (-1) 1 0
    neighbor Above      = hplus $ Hex 0 1 (-1)
    neighbor AboveRight = hplus $ Hex 1 0 (-1)

instance Orientation PointyTop where
    neighbor Right      = hplus $ Hex 1 (-1) 0
    neighbor RightBelow = hplus $ Hex 0 (-1) 1
    neighbor LeftBelow  = hplus $ Hex (-1) 0 1
    neighbor Left       = hplus $ Hex (-1) 1 0
    neighbor LeftAbove  = hplus $ Hex 0 1 (-1)
    neighbor RightAbove = hplus $ Hex 1 0 (-1)

neighbors :: Hexagon ori -> [Hexagon ori]
neighbors hex = map (hplus hex) [Hex x y (-x - y) | x <- [-1, 0, 1], y <- [-1, 0, 1], x /= y]

data Hexagon ori = Hex Integer Integer Integer
                   deriving (Eq, Ord, Show, Generic)

instance Hashable (Hexagon ori)

hplus :: Hexagon ori -> Hexagon ori -> Hexagon ori
(Hex x y z) `hplus` (Hex x1 y1 z1) = Hex (x+x1) (y+y1) (z+z1)

hminus :: Hexagon ori -> Hexagon ori -> Hexagon ori
(Hex x y z) `hminus` (Hex x1 y1 z1) = Hex (x-x1) (y-y1) (z-z1)

hdistance :: Hexagon ori -> Hexagon ori -> Integer
(Hex x y z) `hdistance` (Hex x1 y1 z1) = maximum . map abs $ [x-x1, y-y1, z-z1]

hlerp :: Hexagon ori -> Hexagon ori -> Float -> (Float, Float, Float)
hlerp (Hex x y z) (Hex x1 y1 z1) t =
    let combine :: Integer -> Integer -> Float
        combine p p1 = t * fromInteger (p1 - p) + fromInteger p
    in (combine x x1, combine y y1, combine z z1)

lineBetweenHexagons :: Hexagon ori -> Hexagon ori -> [Hexagon ori]
lineBetweenHexagons a b =
    let dist = hdistance a b
        frac i = (fromInteger i / fromInteger dist :: Float)
    in map (roundToHex . hlerp a b . frac) [0..dist]

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

hexagonsWithinRange :: Hexagon ori -> Integer -> [Hexagon ori]
hexagonsWithinRange (Hex x y z) n = generalizedRangeCollect xs ys zs
    where xs = (x - n, x + n)
          ys = (y - n, y + n)
          zs = (z - n, z + n)

hexagonsReachableWithinRange :: Hexagon ori -> Integer -> (Hexagon ori -> Bool) -> [Hexagon ori]
hexagonsReachableWithinRange start n isBlocked =
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

data RotateDir = CW | CCW deriving (Eq, Ord, Show, Enum, Bounded)

rotateAboutOrigin :: RotateDir -> Hexagon ori -> Hexagon ori
rotateAboutOrigin CW  (Hex x y z) = Hex (-z) (-x) (-y)
rotateAboutOrigin CCW (Hex x y z) = Hex (-y) (-z) (-x)

rotateAroundPoint :: Hexagon ori -> RotateDir -> Hexagon ori -> Hexagon ori
rotateAroundPoint origin rdir h = origin `hplus` rotateAboutOrigin rdir (h `hminus` origin)

isHexagonOnRing :: Hexagon ori -> Integer -> Hexagon ori -> Bool
isHexagonOnRing center radius hex = radius == hex `hdistance` center

ringAroundHexagon :: Hexagon ori -> Integer -> [Hexagon ori]
ringAroundHexagon center 0      = [center]
ringAroundHexagon center radius = ns >>= ringEdge
    where ns = [ Hex 1 (-1) 0
               , Hex 0 (-1) 1
               , Hex (-1) 0 1
               , Hex (-1) 1 0
               , Hex 0 1 (-1)
               , Hex 1 0 (-1)
               ]

          ringEdgeVector (Hex x y z) = Hex z x y

          ringEdge re@(Hex x y z) =
              let start = center `hplus` Hex (radius*x) (radius*y) (radius*z)
              in take (fromInteger radius) . iterate (`hplus` ringEdgeVector re) $ start
