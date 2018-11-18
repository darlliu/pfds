module Main where
import PurelyFunctional
import Prelude hiding (empty, isEmpty, cons, head, tail)


ex_1_1 = PurelyFunctional.powerset $ 4 `cons` (3 `cons` (2 `cons` (singleton 1)))

bt_0 = PurelyFunctional.insert (PurelyFunctional.insert (PurelyFunctional.singlebtree 2) 1) 3
main :: IO ()
main = do
    putStrLn $ show ex_1_1
    putStrLn $ show bt_0