{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}
module NinetyNineProblems where

import Control.Arrow
import Control.Monad
import Data.Function
assoc ((a,b),c) = (a,(b,c))
unassoc (a,(b,c)) = ((a,b),c)
uncurry3 f (a,b,c) = f a b c
curry3 f a b c = f (a,b,c)
uncurry4 f (a,b,c,d) = f a b c d
curry4 f a b c d = f (a,b,c,d)
makeAssoc (a,b,c) = ((a,b),c)
unmakeAssoc ((a,b),c) = (a,b,c)
swap (a,b) = (b,a)
--swap = snd &&& fst
fst4 (a,b,c,d) = a
snd4 (a,b,c,d) = b
trd4 (a,b,c,d) = c
fth4 (a,b,c,d) = d
if'::Bool->a->a->a
if' cond t f = if cond then t else f

--Find the last element of a list.
problem1 = head . reverse

--Find the last but one element of a list. 
problem2 = head . tail . reverse

--Find the K'th element of a list. The first element in the list is number 1. 
problem3 = curry $ first (+(-1)) >>> uncurry drop >>> head

--Find the number of elements of a list.
problem4::[a]->Int
problem4 = foldl (const . succ) 0

--Reverse a list.
problem5::[a]->[a]
problem5 = foldl (flip (:)) [] 

--Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x). 
problem6::(Eq a)=>[a]->Bool
problem6 = problem5 &&& id >>> uncurry (==)

--Flatten a nested list structure. 
--We have to define a new data type, because lists in Haskell are ho`mogeneous.
data NestedList a = NestedList {getData::(Either a [NestedList a])}
problem7::NestedList a -> [a]
problem7 = getData >>> (:[]) ||| (>>=problem7)
--p7test = NestedList {getData = Right [NestedList {getData = Left 1}, NestedList {getData = Right [NestedList {getData = Left 2}, NestedList {getData = Right [NestedList {getData = Left 3}, NestedList {getData = Left 4}]}, NestedList {getData = Left 5}]}]}

--Eliminate consecutive duplicates of list elements.
problem8::(Eq a)=>[a]->[a]
problem8 = ((id &&& tail) >>> uncurry zip >>> filter (uncurry (/=)) >>> map fst) &&& (last >>> (:[])) >>> uncurry (++)
    {- proc lst -> do
    lt <- last -< lst
    tl <- tail -< lst
    pairs <- uncurry zip -< (tl, lst)
    uniques <- filter (uncurry (/=)) -< pairs
    keys <- map fst -< uniques
    final <- uncurry (++) -< (keys, lt)
    returnA -< final
        -}
    {-do
    isNull <- null
    if isNull 
    then return []
    else map (zip . tail
    -}

--Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists. 
problem9::(Eq a)=>[a]->[[a]]
problem9 = (head &&& ((tail &&& id) >>> uncurry zip >>> foldl addOnOrMakeNew [[]] >>> reverse) >>> second (head &&& tail)) >>> unassoc >>> first (uncurry (:)) >>> uncurry (:)
    where
        --addOnOrMakeNew::[[a]]->(a,a)->[[a]]
        addOnOrMakeNew = curry $ (uncurry (==) . snd &&& (fst &&& snd)) >>> second (uncurry trueArrow &&& uncurry falseArrow ) >>> unassoc >>> unmakeAssoc >>> uncurry3 if'
        --Either of these are functions that take (acc, e)->acc; acc::[[a]], e::[a]
        trueArrow::[[a]]->(a,a)->[[a]]
        trueArrow = curry $ first (tail &&& head) >>> second fst >>> assoc >>> second (uncurry $ flip (:)) >>> uncurry (flip (:))
        -- (fst e:(head acc)):tail acc
        falseArrow = curry $ second fst >>> second (:[]) >>> (uncurry . flip)(:)
        -- [fst e]:acc

--Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E
problem10::(Eq a)=>[a]->[(Int,a)]
problem10 = problem9 >>> map (length &&& head)
    --Alternatively, (problem9 >>> map length) &&& problem8 >>> uncurry zip

--Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.
data Run a = Run {getRun :: (Either a (Int, a))} deriving (Show)
problem11::(Eq a)=>[a]->[Run a]
problem11 = problem10 >>> map ( (==1) . fst &&& ( Run . Left . snd &&& Run . Right ) >>> unmakeAssoc . unassoc >>> uncurry3 if')


--Decode a run-length encoded list.
problem12::[Run a]->[a]
problem12 = (=<<) ( getRun >>> ((:[]) ||| (uncurry replicate)))

--Run-length encoding of a list (direct solution).
--Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X. 
problem13::(Eq a)=>[a]->[Run a]
problem13 = problem11 --Screw this; with laziness problem11 is good enough anyway (I think it might satisfy the constraint?).

--Duplicate the elements of a list.
problem14::[a]->[a]
problem14 = (=<<) (replicate 2)

--Replicate the elements of a list a given number of times.
problem15::[a]->Int->[a]
problem15 = (. replicate) . (>>=)
    --curry $ ((>>=) *** take) >>> second (flip (.) repeat) >>> app

--Drop every N'th element from a list. 
problem16::[a] -> Int -> [a]
problem16 = flip $ (id &&& (+) (-1)) >>> uncurry countdownDrop
    where
        countdownDrop = fix . curry4 $ proc input -> do --I am NOT rewriting this in not-do notation.
            let (recurse, base, count, lst) = input
            conditionalInt <- (==) 0 >>> if' -< count --Oddly, functions are not-as-polymorphic when working with arrows,
            conditionalLst <- (==) 0 >>> if' -< count --So I need two of these for Int and [a] respectively.
            nextCount <- first uncurry >>> app -< (conditionalInt, (base-1, count-1))
            recCall <- app -< (uncurry recurse, (base, nextCount))
            excludeVal <- second tail >>> app -< (recCall, lst)
            includeVal <- (head . snd &&& (second tail >>> app)) >>> uncurry (:) -< (recCall, lst)
            nextList <- app -< (uncurry conditionalLst, (excludeVal, includeVal))
            ((null >>> if' >>> uncurry) *** (const [] &&& returnA)) >>> app -< (lst, nextList)
    {-
        countdownDrop::Int->Int->[a]->[a]
        countdownDrop = fix . curry4 $ do
        (recurse, base, count, lst) <- id
        let conditional = if' (count==0)
        let nextCount = conditional (base-1) (count-1)
        let excludeVal = (recurse base nextCount . tail) lst
        let includeVal = (head &&& recurse base nextCount . tail) >>> uncurry (:) $ lst
        let nextList = conditional excludeVal includeVal
        return (if' (null lst) [] nextList)
        -}

--Split a list into two parts; the length of the first part is given. 
problem17::[a]->Int->([a], [a])
problem17 = flip . curry $ first (take &&& drop) >>> duoApp
    where
        duoApp::((d->b, d->c), d)->(b, c)
        duoApp = (first fst >>> app) &&& (first snd >>> app)
        
--Extract a slice from a list.
--Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 1. 
problem18::[a] -> Int -> Int -> [a]
problem18 = curry3 $ makeAssoc >>> assoc >>> swap >>> first ((drop *** take . (+1)) >>> uncurry (.)) >>> app

--Rotate a list N places to the left.
problem19::[a]->Int->[a]
problem19 = curry $ (fst &&& (first length >>> uncurry (flip mod))) >>> uncurry problem17 >>> (uncurry . flip) (++)
    --let trueN = mod n (length a) in
    --                uncurry (++) $ swap $ problem17 a trueN

--Remove the K'th element from a list.
problem20::Int->[a]->[a]
problem20 = curry $ (uncurry . flip) problem17 >>> second tail >>> uncurry (++) 

--Insert an element at a given position into a list.
problem21::a->[a]->Int->[a]

--Create a list containing all integers within a given range.
problem22::(Integral a)=>a->a->[a]


--Extract a given number of randomly selected elements from a list.
problem23::[a]->Int->IO [a]
