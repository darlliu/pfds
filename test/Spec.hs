import Test.HUnit
import PurelyFunctional
import Prelude hiding (empty, isEmpty, cons, head, tail)

myList1  = singleton 1
myList2  = 4 `cons` (3 `cons` (2 `cons` myList1))
myList3  = 3 `cons` (2 `cons` myList1)
myList4  = 3 `cons` (3 `cons` myList1)

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
    (True) (isEmpty SEmpty)

testUpdate =
    TestCase $ assertEqual "Test List: Update List"
    (myList4) (update myList3 3 1)

mySet1 = singlebtree 1
mySet3 = insert (insert mySet1 2) 3

testSetMember = 
    TestCase $ assertEqual "Test List: Member Set"
    (True) (member mySet3 1)

testSetMember2 = 
    TestCase $ assertEqual "Test List: Member Set"
    (False) (member mySet3 4)
main :: IO Counts
main = runTestTT $ TestList [ testHello, testHead, testTail, testIsEmpty, testIsEmpty2,
    testUpdate, testSetMember, testSetMember2 ]