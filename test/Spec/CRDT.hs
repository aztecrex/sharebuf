module Spec.CRDT where
---
import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit (testCase, (@?=))

import Data.Ratio ((%))
import Data.Vector as V

import CRDT

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "CRDT" [

        testCase "initial insert" $ insert mempty 0 'a' @?= (V.singleton (Cell (1 % 2) 'a'), Cell (1 % 2) 'a')

    ]
