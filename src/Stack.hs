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

data List a = Empty | Wrapped a (List a)
    deriving (Show, Read, Ord, Eq)


instance Stack List where
    empty = Empty
    isEmpty s = case s of 
        Empty -> True
        _ -> False
    cons a s = case s of
        Empty -> Wrapped a Empty
        s' -> Wrapped a s'
    head (Wrapped a _) = a
    tail (Wrapped _ s) = s
    tail Empty = empty

singleton :: a -> List a
singleton a = cons a Empty


powerset :: List a -> List (List a)
powerset Empty = Empty
powerset xs = xs `cons` (powerset $ tail xs)