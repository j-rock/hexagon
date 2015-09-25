{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Test.QuickCheck.Modifiers
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.Framework.TH
import           Test.HUnit
import           Data.Hexagon
import qualified Data.HashSet as HS


main :: IO ()
main = $(defaultMainGenerator)


type Prop a = a -> Bool


-- Helps get around Orphan Instances problem
type FakeHexagon t = (Integer, Integer)

fromFake :: FakeHexagon ori -> Hexagon ori
fromFake (x,y) = Hex x y (- x - y)

type RangedHexagon t = (FakeHexagon t, NonNegative Integer)

case_onlySixCornerAndEdgeDirections :: Assertion
case_onlySixCornerAndEdgeDirections = assertBool "Only six directions for corners and edges" onlySixDir
    where onlySixDir = all ((==6) . length) (lr, ab)
          lr = [minBound..maxBound] :: [LeftRightDir]
          ab = [minBound..maxBound] :: [AboveBelowDir]

prop_hexagonIsNeighborOfItsNeighbors :: Prop (FakeHexagon ori)
prop_hexagonIsNeighborOfItsNeighbors fh = all (elem hex) (map neighbors $ neighbors hex)
    where hex = fromFake fh

prop_hexagonsWithinRangeHaveGoodDistance :: Prop (RangedHexagon ori)
prop_hexagonsWithinRangeHaveGoodDistance (fh, NonNegative n) = all ((<= n) . hdistance hex) $ hexagonsWithinRange hex n
    where hex = fromFake fh

prop_unblockedHexagonsWithinRangeAreReachable :: Prop (RangedHexagon ori)
prop_unblockedHexagonsWithinRangeAreReachable (fh, NonNegative n) =
    let hex   = fromFake fh
        range = HS.fromList $ hexagonsWithinRange hex n
        reach = HS.fromList $ hexagonsReachableWithinRange hex n (const False)
    in range == reach


prop_reverseRotationsAreIdentity :: Prop (FakeHexagon ori)
prop_reverseRotationsAreIdentity fh = all (== hex) [cwThenCCW hex, ccwThenCW hex]
    where hex       = fromFake fh
          cwThenCCW = rotateAboutOrigin CCW . rotateAboutOrigin CW
          ccwThenCW = rotateAboutOrigin CW . rotateAboutOrigin CCW

prop_rotationSixTimesIsIdentity :: Prop (FakeHexagon ori)
prop_rotationSixTimesIsIdentity fh = all (== hex) [sixCW hex, sixCCW hex]
  where hex    = fromFake fh
        six f  = head . drop 6 . iterate f
        sixCW  = six $ rotateAboutOrigin CW
        sixCCW = six $ rotateAboutOrigin CCW

prop_ringIsDifferenceBetweenCloseRanges :: Prop (RangedHexagon ori)
prop_ringIsDifferenceBetweenCloseRanges (fh, NonNegative i) = ring == (bigRange `HS.difference` smallRange)
  where   origin     = fromFake fh
          ring       = HS.fromList $ ringAroundHexagon origin i
          bigRange   = HS.fromList $ hexagonsWithinRange origin i
          smallRange = HS.fromList $ hexagonsWithinRange origin (i-1)
