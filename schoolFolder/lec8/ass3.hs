module BTree where
import qualified Data.Tree as T (Tree(..), drawTree)
import Data.Tree.Pretty (drawVerticalTree)

data BTree a = Node a (BTree a) (BTree a) | Leaf deriving Eq

-- pretty print BTree
instance Show a => Show (BTree a) where
    show :: Show a => BTree a -> String
    show = drawVerticalTree . treeconverter


search :: Ord a => a -> BTree a -> Bool
search _ Leaf = False
search x (Node y left right) | x > y = search x right
    | x < y = search x left
    | otherwise = True

-- in its current form, delete does not preserve balance
delete :: Ord a => a -> BTree a -> BTree a
delete a Leaf = error "Element not in the tree"
delete a (Node b left right) | a > b = Node b left (delete a right)
    | a < b = Node b (delete a left) right
    | left == Leaf && right == Leaf = Leaf
    | left == Leaf = right
    | right == Leaf = left
    | otherwise = let z = rightmost left
                    in Node z (delete z left) right
     where
        rightmost :: BTree a -> a
        rightmost Leaf = error "Tree is empty. There's no rightmost node."
        rightmost (Node x Leaf Leaf) = x
        rightmost (Node _ _ right) = rightmost right

-- examples
t1 :: BTree Int
t1 = foldl (flip insert) Leaf [5,4,3,7,1,10,8]
t2 :: BTree Int
t2 = foldl (flip insert) Leaf [1..10]

-- tree traversals
inorder :: BTree a -> [a]
inorder Leaf = []
inorder (Node val left right)= inorder left ++ [val] ++ inorder right

preorder :: BTree a -> [a]
preorder Leaf = []
preorder (Node val left right)= [val] ++ inorder left ++ inorder right

postorder :: BTree a -> [a]
postorder Leaf = []
postorder (Node val left right)= inorder left ++ inorder right ++ [val]

-- conver BTree T.Tree
treeconverter :: Show a => BTree a -> T.Tree String
treeconverter Leaf = T.Node {T.rootLabel = ".", T.subForest =[]}
treeconverter (Node nodeName tL tR) = T.Node {T.rootLabel = (show nodeName), T.subForest = [(treeconverter tL), (treeconverter tR)]}

-- height & balance factor
height :: BTree a -> Int
height Leaf = 0
height (Node v l r) = 1 + max (height l) (height r)


balanceFactor :: BTree a -> Int
balanceFactor Leaf = 0
balanceFactor (Node _ l r) = height l - height r


-- rotations
rotateLeft :: BTree a -> BTree a
rotateLeft (Node x t1 (Node y t2 t3)) = Node y (Node x t1 t2) t3
rotateLeft t = t
rotateRight :: BTree a -> BTree a
rotateRight (Node x (Node y t1 t2) t3) = Node y t1 (Node x t2 t3)

-- rebalance tree
rebalance :: BTree a -> BTree a
rebalance t@(Node x left right)
    | balanceFactor t == 2 && balanceFactor left == -1 = rotateRight $ Node x (rotateLeft left) right
    | balanceFactor t == 2 && balanceFactor left == 0 = rotateRight t
    | balanceFactor t == 2 && balanceFactor left == 1 = rotateRight t
    | balanceFactor t == -2 && balanceFactor right == -1 = rotateLeft t
    | balanceFactor t == -2 && balanceFactor right == 0 = rotateLeft t
    | balanceFactor t == -2 && balanceFactor right == 1 = rotateLeft $ Node x left (rotateRight right)
    | otherwise = t

-- insert preserving balance
insert :: Ord a => a -> BTree a -> BTree a
insert x Leaf = Node x Leaf Leaf
insert x t@(Node y left right) | x > y = rebalance $ Node y left (insert x right)
    | x < y = rebalance $ Node y (insert x left) right
    | otherwise = t

