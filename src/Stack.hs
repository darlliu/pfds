module Stack
where
import Prelude hiding (empty, isEmpty, cons, head, tail)

someFunc = "Hello, World"

class Stack s where
    nullstack :: s a
    isEmpty :: s a -> Bool
    cons :: a -> s a -> s a
    head :: s a -> a
    tail :: s a -> s a 

data List a = SEmpty | Wrapped a (List a)
    deriving (Show, Read, Ord, Eq)


instance Stack List where
    nullstack = SEmpty
    isEmpty s = case s of 
        SEmpty -> True
        _ -> False
    cons a s = case s of
        SEmpty -> Wrapped a SEmpty
        s' -> Wrapped a s'
    head (Wrapped a _) = a
    tail (Wrapped _ s) = s
    tail SEmpty = SEmpty

singleton :: a -> List a
singleton a = cons a SEmpty

powerset :: List a -> List (List a)
powerset SEmpty = SEmpty
powerset xs = xs `cons` (powerset $ tail xs)

update :: List a -> a -> Int -> List a
update SEmpty _ _ = SEmpty
update (Wrapped x xs) x' i
  | i <= 0  = x' `cons` xs
  | otherwise = x `cons` (update xs x' $ i-1)
