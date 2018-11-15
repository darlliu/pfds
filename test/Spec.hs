import Test.HUnit
import PurelyFunctional
import Prelude hiding (empty, isEmpty, cons, head, tail)

myList1  = singleton 1
myList2  = 4 `cons` (3 `cons` (2 `cons` myList1))
myList3  = 3 `cons` (2 `cons` myList1)

testHello =
    TestCase $ assertEqual "Testing basic import"
    (someFunc) (someFunc)

testHead =
    TestCase $ assertEqual "Test List: Head"
    4 (head myList2)

testTail =
    TestCase $ assertEqual "Test List: Tail"
    (myList3) (tail myList2)

testIsEmpty = 
    TestCase $ assertEqual "Test List: IsEmpty"
    (False) (isEmpty myList1)

testIsEmpty2 = 
    TestCase $ assertEqual "Test List: IsEmpty 2"
    (True) (isEmpty Empty)

main :: IO Counts
main = runTestTT $ TestList [ testHello, testHead, testTail, testIsEmpty, testIsEmpty2 ]