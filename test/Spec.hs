import Test.HUnit
import PurelyFunctional

testHello =
    TestCase $ assertEqual "Testing basic import"
    (someFunc) (someFunc)

main :: IO Counts
main = runTestTT $ TestList [ testHello ]