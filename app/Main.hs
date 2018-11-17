module Main where
import PurelyFunctional
import Prelude hiding (empty, isEmpty, cons, head, tail)


ex_1_1 = PurelyFunctional.powerset $ 4 `cons` (3 `cons` (2 `cons` (singleton 1)))

main :: IO ()
main = putStrLn $ show ex_1_1