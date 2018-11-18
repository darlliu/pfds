import Test.HUnit
import qualified Stack
import qualified Set
import Prelude hiding (empty, isEmpty, cons, head, tail)

myList1  = Stack.singleton 1
myList2  = 4 `Stack.cons` (3 `Stack.cons` (2 `Stack.cons` myList1))
myList3  = 3 `Stack.cons` (2 `Stack.cons` myList1)
myList4  = 3 `Stack.cons` (3 `Stack.cons` myList1)

testHello =
    TestCase $ assertEqual "Testing basic import"
    (Stack.someFunc) (Stack.someFunc)

testHead =
    TestCase $ assertEqual "Test List: Head"
    4 (Stack.head myList2)

testTail =
    TestCase $ assertEqual "Test List: Tail"
    (myList3) (Stack.tail myList2)

testIsEmpty = 
    TestCase $ assertEqual "Test List: IsEmpty"
    (False) (Stack.isEmpty myList1)

testIsEmpty2 = 
    TestCase $ assertEqual "Test List: IsEmpty 2"
    (True) (Stack.isEmpty Stack.Nil)

testUpdate =
    TestCase $ assertEqual "Test List: Update List"
    (myList4) (Stack.update myList3 3 1)

mySet1 = Set.singleton 1
mySet3 = Set.insert (Set.insert mySet1 2) 3

testSetMember = 
    TestCase $ assertEqual "Test List: Member Set"
    (True) (Set.member mySet3 1)

testSetMember2 = 
    TestCase $ assertEqual "Test List: Member Set"
    (False) (Set.member mySet3 4)
main :: IO Counts
main = runTestTT $ TestList [ testHello, testHead, testTail, testIsEmpty, testIsEmpty2,
    testUpdate, testSetMember, testSetMember2 ]