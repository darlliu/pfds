module Set 
where
import Prelude hiding (empty, isEmpty, cons, head, tail, null)

class Set s where
    null :: s a
    member :: (Ord a) => s a -> a -> Bool
    insert :: (Ord a) => s a -> a -> s a

data Ord a => BTree a = Nil | BTree {
    left :: BTree a,
    right :: BTree a,
    ele :: a
} deriving (Show, Read)

singleton :: (Ord a) => a -> BTree a
singleton a = BTree {
    left = Nil,
    right = Nil,
    ele = a
} 

instance Set BTree where
    null = Nil
    member Nil x = False
    member BTree {left=tl, right=tr, ele=e} x 
      | e > x = member tr x
      | e < x = member tl x
      | otherwise = True
    insert Nil x = singleton x
    insert BTree {left=tl, right=tr, ele=e} x
      | e < x = BTree {left=tl, right=insert tr x, ele=e}
      | e > x = BTree {left=insert tl x, right=insert tr x, ele=e}
      | otherwise = BTree {left=tl, right=tr, ele=e}