{-# LANGUAGE NumDecimals #-}

module Test.Pos.Crypto.Golden
    ( tests
    ) where

import           Universum

import           Hedgehog

import           Pos.Binary.Class (Cons (..), deriveSimpleBi)

import           Test.Pos.Binary.Tripping (discoverGolden, embedGoldenTest, goldenTestBi)

data MyData = MyData deriving (Eq, Show)

deriveSimpleBi ''MyData [ Cons 'MyData [] ]

golden_MyData :: Property
golden_MyData = goldenTestBi MyData $(embedGoldenTest "MyData")

tests :: IO Bool
tests = checkSequential $$discoverGolden
