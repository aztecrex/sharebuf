
import Test.Tasty (TestTree, testGroup, defaultMain)
import qualified Spec.CRDT as CRDT (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "all" [
    CRDT.tests
    ]
