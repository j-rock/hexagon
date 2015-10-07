{-# LANGUAGE DeriveGeneric #-}


module Data.Hexagon.Hexagon where

import           Data.Hashable (Hashable)
import           GHC.Generics  (Generic)

-- | The fundamental coordinate type for a 2D hexagonal grid.
--
-- @
-- Although the constructor is exported, be warned that incorrect
-- instantiations of a Hexagon will yield undefined results.
-- That is, for a Hexagon (Hex x y z), it must be that x + y + z = 0.
-- See Data.Hexagon.Coordinate for safe constructors.
-- @
data Hexagon ori = Hex Integer Integer Integer
                   deriving (Eq, Ord, Show, Generic)

instance Hashable (Hexagon ori)

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

-- | Multiplies the corresponding components of the Hexagon with the given scalar.
scale :: Hexagon ori -> Integer -> Hexagon ori
scale (Hex x y z) k = Hex (k * x) (k * y) (k * z)
