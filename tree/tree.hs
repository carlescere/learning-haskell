data BinTree a = Nil | Node (BinTree a) a (BinTree a) deriving Show

insert :: Ord a => a -> BinTree a -> BinTree a
insert x Nil = Node Nil x Nil
insert x (Node l v r)
  | x > v     = Node l v (insert x r)
  | otherwise = Node (insert x l) v r

isEmpty :: Ord a => BinTree a -> Bool
isEmpty Nil = True
isEmpty _ = False

isLeaf :: Ord a => BinTree a -> Bool
isLeaf (Node Nil v Nil) = True
isLeaf _ = False

exist :: Ord a => a -> BinTree a -> Bool
exist _ Nil = False
exist x (Node l v r) = x == v || exist x dir
  where dir
          | x > v     = r
          | otherwise = l

toArr :: Ord a => BinTree a -> [a]
toArr Nil = []
toArr (Node l v r) = toArr l ++ v : toArr r
