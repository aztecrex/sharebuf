module Spec.CRDT where
---
import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit (testCase, (@?=))

import Data.Ratio ((%))
import Data.Set as S (fromList)
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

        testCase "insert after all" $
            let orig = insert' mempty 0 'a'
                actual = insert' orig 1 'b'
            in emit actual @?= "ab",

        testCase "insert way after" $
            let orig = insert' mempty 0 'a'
                actual = insert' orig 100 'b'
            in emit actual @?= "ab",

        testCase "insert before all" $
            let orig = insert' mempty 0 'a'
                actual = insert' orig 0 'b'
            in emit actual @?= "ba",

        testCase "insert between" $
            let orig = insert' (insert' mempty 0 'a') 1 'b'
                actual = insert' orig 1 'c'
            in emit actual @?= "acb",

        testCase "unique identifiers" $
            let x = insert' (insert' mempty 1 'a') 1 'a'
                y = insert' (insert' x 0 'a') 0 'a'
                actual = insert' (insert' y 2 'a') 1 'a'
                unique = S.fromList . V.toList . fmap uid $ actual
            in Prelude.length unique @?= 6,

        testCase "increasing identifiers" $
        let x = insert' (insert' mempty 1 'a') 1 'a'
            y = insert' (insert' x 0 'a') 0 'a'
            actual = insert' (insert' y 2 'a') 1 'a'
        in snd (Prelude.foldl ascending (0, True) (fmap uid actual)) @?= True
    ]

ascending :: (Ord a) => (a, Bool) -> a -> (a, Bool)
ascending (x, True) y = if y > x then (y, True) else (y, False)
ascending _ y = (y, False)

insert' :: Buffer -> Int -> Char -> Buffer
insert' cs i c = fst $ insert cs i c

