module Graphs where

class Graph v where
    -- add something here
    vertices::v a->v a
    vertices = id
    edges::v a->v (a, a)

instance Graph [] where
    edges l = [(x,y) | x<-l, y<-l]

class Node n where
    value::n a->a
    children::n a->[n a]

class (Node aNode) => Tree aNode where
    root::aNode a->a
    root = value
    --The idea is that if we know that it's a tree, it's acyclical
    --TODO: Graph v => edges

{-
class (Tree treeType) => BinaryTree treeType where
    isNil::treeType b->Bool
    left::treeType b->treeType b
    right::treeType b->treeType b

data BinarySearchTree a = BSTNil | BST a (BinarySearchTree a) (BinarySearchTree a) deriving (Show)
instance BinaryTree BinarySearchTree where
    isNil BSTNil = True
    isNil _ = False
    
    left 

data Heap a = HeapNil | Heap a (Heap a) (Heap a) deriving (Show)
-}

class (Tree treeType) => BinaryTreeType treeType where
    isNil::treeType a->Bool
    isNil = isNil . getTree
    left::treeType a->treeType a
    left = makeTree . left . getTree
    right::treeType a->treeType a
    right = makeTree . right . getTree
    makeTree::BinaryTree a->treeType a
    getTree::treeType a->BinaryTree a
    
    isFull::treeType a->Bool
    isFull t
        | isNil t   = True
        | otherwise = height (left t) == height (right t) && isFull (left t) && isFull (right t)
    
    isComplete::treeType a->Bool
    isComplete t
        | isNil t   = True
        | otherwise = (height (left t) == height (right t) || height (left t) == height (right t) +1) && isComplete (left t) && isComplete(right t)

data BinaryTree a = Nil | Node a (BinaryTree a) (BinaryTree a) deriving (Show)
instance BinaryTreeType BinaryTree where
    isNil Nil = True
    isNil _ = False

    left (Node a l r) = l
    left Nil = error "Tried to go left of Nil."

    right (Node a l r) = r
    right Nil = error "Tried to go right of Nil."
    
    makeTree = id
    getTree = id
    
    

instance (Eq a) => Eq (BinaryTree a) where
    Nil == Nil = True
    --pre-order checking
    Node root1 left1 right1 == Node root2 left2 right2 = (root1 == root2) && (left1 == left2) && (right1 == right2)
    _ == _ = False
    
instance Node BinaryTree where
    value (Node a left right) = a
    value Nil = error "Tried to find the root of Nil."
    children (Node a left right) = filter (not . isNil) [left, right]
    children Nil = error "Tried to find the children of Nil."
instance Tree BinaryTree

size::(Tree a, Integral b)=>a c->b
size tree = 1 + (sum . map size) (children tree)

height::(Tree a, Integral b)=>a c->b
height tree = if length (children tree) == 0 then 1 else 1 + (foldl1 max . map height . children) tree

--BST

newtype BinarySearchTree a = BinarySearchTree (BinaryTree a) deriving (Show)
type BST = BinarySearchTree
instance BinaryTreeType BinarySearchTree where
    getTree (BinarySearchTree x) = x
    makeTree x = BinarySearchTree x
instance Node BinarySearchTree where
    value = value . getTree
    children = map makeTree . children . getTree
instance Tree BinarySearchTree


insertInBST::(Ord a)=>BST a->a->BST a
insertInBST (BinarySearchTree Nil) v = BinarySearchTree (Node v Nil Nil)
insertInBST (BinarySearchTree (Node n l r)) v
    | v <= n    = makeTree (Node n (getTree $ insertInBST (makeTree l) v) r)
    | otherwise = makeTree (Node n l (getTree $ insertInBST (makeTree r) v))

listToBinarySearchTree::(Ord a)=>[a]->BST a
listToBinarySearchTree = foldl insertInBST polymorphicNil

polymorphicNil::(BinaryTreeType a)=>a b
polymorphicNil = makeTree Nil
instance BinaryTreeType Heap where
    getTree (Heap x) = x
    makeTree x = Heap x
newtype Heap a = Heap (BinaryTree a) deriving (Show)

instance Node Heap where
    value = value . getTree
    children = map makeTree . children . getTree
instance Tree Heap

insertIntoHeap::(Ord a)=>Heap a->a->Heap a
insertIntoHeap (Heap Nil) v = Heap (Node v Nil Nil) 
insertIntoHeap (Heap (Node n Nil Nil)) r = if r<n 
    then Heap (Node r (Node n Nil Nil) Nil)
    else Heap (Node n (Node r Nil Nil) Nil)
insertIntoHeap (Heap (Node n l Nil)) r = if r<n
    then Heap (Node r l (Node n Nil Nil))
    else Heap (Node n l (Node r Nil Nil))
insertIntoHeap (Heap (Node n l r)) v = 
    if (isFull (Node n l r)) || height l > height r && not (isFull left)
        then Heap (Node lesserRoot (getTree $ insertIntoHeap (makeTree l) greaterRoot) r)
        else Heap (Node lesserRoot l (getTree $ insertIntoHeap (makeTree r) greaterRoot))
    where
        lesserRoot = min v n
        greaterRoot = max v n

{-

data RedBlackTree = Red a (RedBlackTree a) (RedBlackTree a) | Black a (RedBlackTree a) (RedBlackTree a) | Black EmptyRedBlackTree
instance BinaryTreeType RedBlackTree where
    getTree (Black EmptyRedBlackTree) = Nil
    getTree (Red x left right) = Node x (getTree left) (getTree right)
    getTree (Black x left right) = x
    makeTree x = (Black x) -- Do not use until I make an alorithm for balancing
instance Node RedBlackTree where
    value = value . getTree
    children = map makeTree . children . getTree
instance Tree Heap

insertInRedBlack::(Ord a)->RedBlackTree a->a->RedBlackTree a
insertInRedBlack (Black EmptyTree) element = Red
-}
--really too tedious for me to continue, but
--I think I'd have to look ahead to a depth of two,
--And recolor based on what I see.