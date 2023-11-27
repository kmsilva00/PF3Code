import qualified Data.Tree as T (Tree(..), drawTree)
import Data.Tree.Pretty (drawVerticalTree)
import qualified Data.Map.Internal.Debug as T

data BTree a = Node a (BTree a) (BTree a) | Leaf deriving Eq --(Show, Eq)


instance Show a => Show(BTree a) where
    show :: Show a => BTree a -> String
    show = drawVerticalTree . treeconverter

search' :: Ord a => a -> BTree a -> Bool
search' _ Leaf = False
search' x (Node y left right) 
    | x > y = search' x right
    | x < y = search' x left
    | otherwise = True

insert :: Ord a => a -> BTree a -> BTree a
insert x Leaf = Node x Leaf Leaf
insert x t@(Node y left right)  | x > y = Node y left (insert x right)
                                | x < y = Node y (insert x left) right
                                | otherwise = t

delete :: Ord a => a -> BTree a -> BTree a
delete a Leaf = error "Element not in the tree"
delete a (Node b left right)    | a > b = Node b left (delete a right)
                                | a < b = Node b (delete a left) right
                                | left == Leaf && right == Leaf = Leaf -- from here, a == b
                                | left == Leaf = right
                                | right == Leaf = left
                                | otherwise = let z = rightmost left
                                    in Node z (delete z left) right
                                        where
                                            rightmost :: BTree a -> a
                                            rightmost Leaf = error "Tree is empty. There's no rightmost node."
                                            rightmost (Node x Leaf Leaf) = x
                                            rightmost (Node _ _ right) = rightmost right
                                                where
                                                    rightmost :: BTree a -> a
                                                    rightmost Leaf = error "Tree is empty. There's no rightmost node."
                                                    rightmost (Node x Leaf Leaf) = x
                                                    rightmost (Node _ _ right) = rightmost right

---

inorder :: BTree a -> [a]
inorder Leaf = []
inorder (Node val left right)= inorder left ++ [val] ++ inorder right


preorder :: BTree a -> [a]
preorder Leaf = []
preorder (Node val left right)= [val] ++ inorder left ++ inorder right

postorder :: BTree a -> [a]
postorder Leaf = []
postorder (Node val left right)= inorder left ++ inorder right ++ [val]

---

-- T.Tree
-- data Tree a = Node a [Tree a]

-- treeconverter :: BTree String-> T.Tree String
-- treeconverter Leaf = T.Node {T.rootLabel = "Leaf"  , T.subForest =[]}
-- treeconverter (Node nodeName tL tR) = T.Node {T.rootLabel = nodeName, T.subForest = [(treeconverter tL), (treeconverter tR)]}


-- btree2tree :: BTree a -> T.Tree a
-- btree2tree (Node val Leaf Leaf) = Node val [] 
-- btree2tree (Node val left Leaf) = Node val [left] -- left here is a BTree of a instead of T.Tree
-- bbtree2tree (Node val Leaf right) = Node val [right]
-- btree2tree (Node val left right) = Node val [left,right]

--idk if it works
-- btree2tree :: BTree a -> T.Tree a
-- btree2tree (Node x Leaf Leaf) = T.Node x []
-- btree2tree (Node x Leaf right) = T.Node x [btree2tree right]
-- btree2tree (Node x left Leaf) = T.Node x [btree2tree left]
-- btree2tree (Node x left right) = T.Node x [btree2tree left, btree2tree right]

-- btree2tree :: Eq a => BTree a -> Maybe (T.Tree a)
-- btree2tree Leaf = Nothing
-- btree2tree (Node v l r) =
--     Just T.Node { T.rootLabel = v, T.subForest = map fromJust $ filter (/=Nothing) $ map btree2tree [l, r] }


treeconverter :: Show a => BTree a-> T.Tree String
treeconverter Leaf = T.Node {T.rootLabel = "Leaf"  , T.subForest =[]}
treeconverter (Node nodeName tL tR) = T.Node {T.rootLabel = show(nodeName), T.subForest = [(treeconverter tL), (treeconverter tR)]}

--t1 = foldl (flip insert) Leaf [5,4,3,7,1,10,8]
--putStr $ drawVerticalTree $ treeconverter t1

-- Teacher put Show a => BTree a, BTree to be an instance of type class show, 
-- T.Tree type of tree from lib
-- make data type BTree, instead of deriving instance for Show, to be instance of type class show 

-- he wants to define show in B Tree as  an instance Show something, on top


--AVL Trees
-- Height

height :: BTree a -> Int
height Leaf = 0
height (Node v l r) = 1 + max (height l) (height r)

balanceFactor :: BTree a -> Int
balanceFactor (Leaf) = 0
balanceFactor (Node _ l r) = (height l) - (height r)

rotateLeft :: BTree a -> BTree a
rotateLeft (Node x t1 ( Node y t2 t3 )) = Node y (Node x t1 t2) t3

rotateRight :: BTree a -> BTree a   

