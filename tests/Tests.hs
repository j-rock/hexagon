{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.Framework.TH
import           Test.HUnit
import           Test.QuickCheck.Modifiers


main :: IO ()
main = $(defaultMainGenerator)


type Prop a = a -> Bool

{-
- prop_blah :: Prop x
-
- case_blah :: Assertion
-}

prop_success :: Prop ()
prop_success = const True

case_success :: Assertion
case_sucesss = assertTrue True
