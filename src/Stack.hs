module Stack
where
import Prelude hiding (empty, isEmpty, cons, head, tail, null)

someFunc = "Hello, World"

class Stack s where
    null :: s a
    isEmpty :: s a -> Bool
    cons :: a -> s a -> s a
    head :: s a -> a
    tail :: s a -> s a 

data List a = Nil | Wrapped a (List a)
    deriving (Show, Read, Ord, Eq)


instance Stack List where
    null = Nil
    isEmpty s = case s of 
        Nil -> True
        _ -> False
    cons a s = case s of
        Nil -> Wrapped a Nil
        s' -> Wrapped a s'
    head (Wrapped a _) = a
    tail (Wrapped _ s) = s
    tail Nil = Nil

singleton :: a -> List a
singleton a = cons a Nil

powerset :: List a -> List (List a)
powerset Nil = Nil
powerset xs = xs `cons` (powerset $ tail xs)

update :: List a -> a -> Int -> List a
update Nil _ _ = Nil
update (Wrapped x xs) x' i
  | i <= 0  = x' `cons` xs
  | otherwise = x `cons` (update xs x' $ i-1)
