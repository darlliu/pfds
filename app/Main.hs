module Main where
import qualified Stack
import qualified Set
import Prelude hiding (empty, isEmpty, cons, head, tail)


ex_1_1 = Stack.powerset $ 4 `Stack.cons` (3 `Stack.cons` (2 `Stack.cons` (Stack.singleton 1)))

bt_0 = Set.insert (Set.insert (Set.singleton 2) 1) 3
main :: IO ()
main = do
    putStrLn $ show ex_1_1
    putStrLn $ show bt_0