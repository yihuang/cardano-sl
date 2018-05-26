{-# LANGUAGE NumDecimals #-}

module Test.Pos.Crypto.Golden
    ( tests
    ) where

import           Universum

import           Hedgehog

import           Pos.Binary.Class (Cons (..), deriveSimpleBi)

import           Test.Pos.Binary.Tripping (embedGoldenTest, goldenTestBi)

data MyData = MyData deriving (Eq, Show)

deriveSimpleBi ''MyData [ Cons 'MyData [] ]

prop_golden_MyData :: Property
prop_golden_MyData = goldenTestBi MyData $(embedGoldenTest "prop_golden_MyData")

tests :: IO Bool
tests = checkParallel $$discover
