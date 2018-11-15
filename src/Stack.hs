module Stack
where
import Prelude hiding (empty, isEmpty, cons, head, tail)

someFunc = "Hello, World"

class Stack s where
    empty :: s a
    isEmpty :: s a -> Bool
    cons :: a -> s a -> s a
    head :: s a -> a
    tail :: s a -> s a 

data List a = Empty | MyCons a (List a)
    deriving (Show, Read, Ord, Eq)


instance Stack List where
    empty = Empty
    isEmpty s = case s of 
        Empty -> True
        _ -> False
    cons a s = case s of
        Empty -> MyCons a Empty
        s' -> MyCons a s'
    head (MyCons a _)  = a
    tail Empty = empty
    tail (MyCons _ s) = s

singleton :: a -> List a
singleton a = cons a Empty