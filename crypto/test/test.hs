import           Universum

import           Test.Hspec (hspec)

import           Test.Pos.Binary.Tripping (runTests)

import           Spec (spec)
import qualified Test.Pos.Crypto.Golden as Golden

main :: IO ()
main = do
    hspec spec
    runTests [ Golden.tests ]
