doubleMe x = x+x

factorial x = if x>0
    then x*factorial (x-1)
    else 1

myList = [0,1,2,3,4,5,6,7,8,9]
mySquares = [0,1,4,9,16,25,36,49,64,81]

length' lst = length_rec lst 0
length_rec lst acc =
    if cond
        then acc
        else length_rec (tail lst) (succ acc)
    where cond = null lst

triplePrint tup =
    [fst3 tup, snd3 tup, trd3 tup]
    where
        fst3::(a,b,c) -> a
        fst3 (x,_,_) = x
        snd3::(a,b,c) -> b
        snd3 (_,x,_) = x
        trd3::(a,b,c) -> c
        trd3 (_,_,x) = x

get_col::[[a]]->Int->[a]
get_col mat col = map (!!col) mat
transpose::[[a]]->[[a]]
transpose mat = map (get_col mat) $ take (length mat) [0..]
myMat = [[0,1,2],[3,4,5],[6,7,8]]

isSomething (Just x)  = True
isSomething x = False

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x rest = treeInsert_rec x rest (\x->x)
treeInsert_rec x EmptyTree cont = cont $ singleton x
treeInsert_rec x (Node a left right) cont
    | x == a = cont $ Node x left right
    | x  < a = treeInsert_rec x left (\acc->cont $ Node a acc right)
    | x  > a = treeInsert_rec x right (\acc->cont $ Node a left acc)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right

makeTree::(Ord a)=>[a]->Tree a
makeTree = \lst->(foldr treeInsert EmptyTree lst)

size::(Ord a, Integral b)=>Tree a->b
size = \tree->(size_rec tree 0 (\x->x))
size_rec EmptyTree acc cont = cont acc
size_rec (Node a left right) acc cont = size_rec left (succ acc) (\acc->size_rec right acc cont)

nums = [8,6,4,1,7,3,5]
numsTree = foldr treeInsert EmptyTree nums









class YesNo a where
    yesno::a->Bool

instance (YesNo a)=>YesNo (Maybe a) where
    yesno (Just a) = yesno a
    yesno Nothing = False

{-
instance YesNo (Maybe a) where
    yesno (Just a) = True
    yesno Nothing = False
-}

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Char where
    yesno '\NUL' = False
    yesno _ = True




