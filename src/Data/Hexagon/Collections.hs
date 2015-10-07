module Data.Hexagon.Collections
    (
      lineTo
    , range
    , rangeWithBlocked
    , ringAround
    ) where

import           Data.Hexagon.Hexagon
import           Data.Hexagon.Orientation (neighbors)

import           Control.Monad            (forM_, replicateM_, unless)
import           Control.Monad.ST         (runST)
import qualified Data.HashTable.Class     as HT
import qualified Data.HashTable.ST.Cuckoo as HTC
import           Data.Maybe               (isJust)
import qualified Data.STRef               as ST


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
              let start = center `plus` (re `scale` radius)
              in take (fromInteger radius) . iterate (`plus` ringEdgeVector re) $ start
