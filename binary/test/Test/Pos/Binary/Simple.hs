{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE NumDecimals     #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Pos.Binary.Simple
    ( tests
    ) where

import           Universum

import           Data.Text.Buildable (Buildable (..))
import qualified Data.Text.Internal.Builder as Builder

import           Hedgehog (Gen, Property, discover)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Pos.Binary.Class (Cons (..), Field (..), cborError, deriveSimpleBi)

import           Test.Pos.Binary.Tripping (discoverGolden, embedGoldenTest, goldenTestBi,
                                           trippingBiBuildable, trippingBiShow)

data Test
    = TestInt { a :: Int }
    | TestIntList { b :: [Int] }
    | TestChar2 { c :: Char, d :: Char }
    | TestInteger { e :: Integer }
    | TestMaybeInt { f :: Maybe Int }
    deriving (Eq, Show, Typeable)


deriveSimpleBi ''Test [
    Cons 'TestInt [
        Field [| a :: Int       |]
        ],
    Cons 'TestIntList [
        Field [| b :: [Int]     |]
        ],
    Cons 'TestChar2 [
        Field [| c :: Char      |],
        Field [| d :: Char      |]
        ],
    Cons 'TestInteger [
        Field [| e :: Integer   |]
        ],
    Cons 'TestMaybeInt [
        Field [| f :: Maybe Int |]
        ]
    ]

instance Buildable Test where
    build = Builder.fromString . show


genTest :: Gen Test
genTest =
    Gen.choice
        [ TestInt <$> Gen.int Range.constantBounded
        , TestIntList <$> Gen.list (Range.linear 0 20) (Gen.int Range.constantBounded)
        , TestChar2 <$> Gen.unicode <*> Gen.unicode
        , TestInteger <$> Gen.integral (Range.linear (- bignum) bignum)
        , TestMaybeInt <$> Gen.maybe (Gen.int Range.constantBounded)
        ]
  where
    bignum = 2 ^ (80 :: Integer)

golden_TestInt :: Property
golden_TestInt = goldenTestBi (TestInt 42) "8200182a"

golden_TestIntList :: Property
golden_TestIntList = goldenTestBi (TestIntList [1, 3, 5, 7]) "82019f01030507ff"

golden_TestChar2 :: Property
golden_TestChar2 = goldenTestBi (TestChar2 '\0' 'a') "830261006161"

golden_TestInteger :: Property
golden_TestInteger = goldenTestBi (TestInteger 123456789123456789123456789)
                                  "8203c24b661efdf2e3b19f7c045f15"

golden_TestBigInteger :: Property
golden_TestBigInteger = goldenTestBi (TestInteger 2e50)
                                     $(embedGoldenTest "TestBigInteger")

golden_TestMaybeIntNothing :: Property
golden_TestMaybeIntNothing = goldenTestBi (TestMaybeInt Nothing) "820480"

golden_TestMaybeIntJust :: Property
golden_TestMaybeIntJust = goldenTestBi (TestMaybeInt (Just 42)) "820481182a"

-- -----------------------------------------------------------------------------

prop_round_trip_derived_bi_show_instance :: Property
prop_round_trip_derived_bi_show_instance =
    H.withTests 5000 . H.property $
        trippingBiShow =<< H.forAll genTest

prop_round_trip_derived_bi_buildable_instance :: Property
prop_round_trip_derived_bi_buildable_instance =
    H.withTests 5000 . H.property $
        trippingBiBuildable =<< H.forAll genTest


-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
    (&&) <$> H.checkSequential $$discoverGolden <*> H.checkParallel
        $$discover
