data BinTree a = Nil | Node (BinTree a) a (BinTree a) deriving Show

insert :: Ord a => a -> BinTree a -> BinTree a
insert x Nil = Node Nil x Nil
insert x (Node l v r) = if x > v then Node l v (insert x r) else Node (insert x l) v r

isEmpty :: Ord a => BinTree a -> Bool
isEmpty Nil = True
isEmpty _ = False

isLeaf :: Ord a => BinTree a -> Bool
isLeaf (Node Nil v Nil) = True
isLeaf _ = False

exist :: Ord a => a -> BinTree a -> Bool
exist _ Nil = False
exist x (Node l v r) = x == v || if x > v then exist x r else exist x l

toArr :: Ord a => BinTree a -> [a]
toArr Nil = []
toArr (Node l v r) = toArr l ++ v : toArr r
