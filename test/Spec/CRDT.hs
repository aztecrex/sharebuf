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

        testCase "initial insert" $
            insert mempty 0 'a' @?= (V.singleton (Cell (1 % 2) 'a'), Cell (1 % 2) 'a'),


        testCase "emit as text" $
            let buf = V.fromList [Cell (1 % 4) 'a', Cell (1 % 2) 'b', Cell (3 % 4) 'c']
            in emit buf @?= "abc",

    ]




insert' :: Buffer -> Int -> Char -> Buffer
insert' cs i c = fst $ insert cs i c

