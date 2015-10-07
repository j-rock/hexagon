module Data.Hexagon.Coordinate
    (
      HexCoordTuple(..)
    , OddR(..)
    , EvenR(..)
    , OddQ(..)
    , EvenQ(..)
    , Axial(..)
    ) where

import           Data.Hexagon.Hexagon

import qualified Data.Bits            as Bits

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
