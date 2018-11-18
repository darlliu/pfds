module Set 
where
import Prelude hiding (empty, isEmpty, cons, head, tail)

class Set s where
    nullset :: s a
    member :: (Ord a) => s a -> a -> Bool
    insert :: (Ord a) => s a -> a -> s a

data Ord a => BTree a = BTEmpty | BTree {
    left :: BTree a,
    right :: BTree a,
    ele :: a
} deriving (Show, Read)

singlebtree :: (Ord a) => a -> BTree a
singlebtree a = BTree {
    left = BTEmpty,
    right = BTEmpty,
    ele = a
} 

instance Set BTree where
    nullset = BTEmpty
    member BTEmpty x = False
    member BTree {left=tl, right=tr, ele=e} x 
      | e > x = member tr x
      | e < x = member tl x
      | otherwise = True
    insert BTEmpty x = singlebtree x
    insert BTree {left=tl, right=tr, ele=e} x
      | e < x = BTree {left=tl, right=insert tr x, ele=e}
      | e > x = BTree {left=insert tl x, right=insert tr x, ele=e}
      | otherwise = BTree {left=tl, right=tr, ele=e}