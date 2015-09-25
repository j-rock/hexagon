{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.HashSet                         as HS
import           Data.Hexagon
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.Framework.TH
import           Test.HUnit
import           Test.QuickCheck.Modifiers


main :: IO ()
main = $(defaultMainGenerator)


type Prop a = a -> Bool


-- Helps get around Orphan Instances problem
type FakeHexagon t = (Integer, Integer)
data P t = P

fromFake :: FakeHexagon t -> Hexagon ori
fromFake = toHexagon (P :: P Axial)

type RangedHexagon t = (FakeHexagon t, NonNegative Integer)

case_onlySixCornerAndEdgeDirections :: Assertion
case_onlySixCornerAndEdgeDirections = assertBool "Only six directions for corners and edges" onlySixDir
    where onlySixDir = all ((==6) . length) (lr, ab)
          lr = [minBound..maxBound] :: [LeftRightDir]
          ab = [minBound..maxBound] :: [AboveBelowDir]

prop_hexagonIsNeighborOfItsNeighbors :: Prop (FakeHexagon ori)
prop_hexagonIsNeighborOfItsNeighbors fh = all (elem hex) (map neighbors $ neighbors hex)
    where hex = fromFake fh

prop_straightLinePathHasRightDistance :: Prop (FakeHexagon ori, FakeHexagon ori)
prop_straightLinePathHasRightDistance (fs, fe) = fromInteger (start `dist` end) + 1 == length (start `lineTo` end)
    where start = fromFake fs
          end   = fromFake fe

prop_hexagonsWithinRangeHaveGoodDistance :: Prop (RangedHexagon ori)
prop_hexagonsWithinRangeHaveGoodDistance (fh, NonNegative n) = all ((<= n) . dist hex) $ range hex n
    where hex = fromFake fh

prop_unblockedHexagonsWithinRangeAreReachable :: Prop (RangedHexagon ori)
prop_unblockedHexagonsWithinRangeAreReachable (fh, NonNegative n) =
    let hex     = fromFake fh
        ranged  = HS.fromList $ range hex n
        rBlock  = HS.fromList $ rangeWithBlocked hex n (const False)
    in ranged == rBlock


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
          ring       = HS.fromList $ ringAround origin i
          bigRange   = HS.fromList $ range origin i
          smallRange = HS.fromList $ range origin (i-1)

forwardConversion :: HexCoordTuple t => proxy t -> Prop (FakeHexagon ori)
forwardConversion p fh = fh == fromHexagon p (toHexagon p fh)

prop_oddrForwardConversionsAreIdentity :: Prop (FakeHexagon ori)
prop_oddrForwardConversionsAreIdentity = forwardConversion (P :: P OddR)

prop_evenrForwardConversionsAreIdentity :: Prop (FakeHexagon ori)
prop_evenrForwardConversionsAreIdentity = forwardConversion (P :: P EvenR)

prop_oddqForwardConversionsAreIdentity :: Prop (FakeHexagon ori)
prop_oddqForwardConversionsAreIdentity = forwardConversion (P :: P OddQ)

prop_evenqForwardConversionsAreIdentity :: Prop (FakeHexagon ori)
prop_evenqForwardConversionsAreIdentity = forwardConversion (P :: P EvenQ)

prop_axialForwardConversionsAreIdentity :: Prop (FakeHexagon ori)
prop_axialForwardConversionsAreIdentity = forwardConversion (P :: P Axial)
