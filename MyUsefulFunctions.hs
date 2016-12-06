--Stuff I can see having a use commonly that isn't by default in Prelude
--Crazycolorz5@gmail.com
--Last Updated: 21 October 2016

--(The thing about me sticking everything in one place is that
-- I can write a very fast draft/working version of a function,
-- then go back and optimize it later if it's necessary.
-- factorial, below, is a good example of this)

--TODO: TCO things, use foldl' (import)

module MyUsefulFunctions (factorial, primes, isPrime, digitsBase, digits, asBase, bits, digitsToInt, maxList, minList, maxList', minList', isSorted, insertionSort, quickSort, quickerSort, perms, powerSet, cartesianProduct, count, countTrue, makeCounts, (<-$), (<<=)) where

import qualified Data.List as List (foldl1', foldl')
import qualified Data.Map as Map

factorial::(Integral a)=>a->a
factorial n = factorial_rec n 1
factorial_rec::(Integral a)=>a->a->a --TCO
factorial_rec 0 acc = acc
factorial_rec n acc = factorial_rec (pred n) (n*acc)

primes::(Integral a)=>[a]
primes = 2:3:5:7:11:13:17:19:filter isPrime [23..]

isPrime::(Integral a)=>a->Bool
isPrime n = (n > 1) && (not . any (\p->mod n p == 0) . 
    takeWhile (<= (floor . sqrt. fromIntegral) n) $ primes)
    --Concerned about floating point error for the perfect square of very large primes

digitsBase::(Integral a)=>a->a->[a]
digitsBase b = reverse . digitsBase_helper b
digitsBase_helper::(Integral a)=>a->a->[a]
digitsBase_helper b n = if n<b then [n] else (mod n b):(digitsBase_helper b (div n b))

digits::(Integral a)=>a->[a]
digits = digitsBase 10

asBase::(Integral a)=>[a]->a->a
asBase l b = asBase_helper l b 0
asBase_helper::(Integral a)=>[a]->a->a->a
asBase_helper [] _ acc = acc
asBase_helper x b acc = asBase_helper (tail x) b (b*acc+(head x))

bits::(Integral a)=>a->[a]
bits = digitsBase 2

digitsToInt::(Integral a)=>[a]->a
digitsToInt = List.foldl' (\acc->(\digit->acc*10+digit)) 0 

maxList::(Foldable t, Ord a) => t a -> a
maxList l
  | null l = error "Tried to find the maximum of an empty foldable."
  | otherwise = foldl1 max l
minList::(Foldable t, Ord a) => t a -> a
minList l
  | null l = error "Tried to find the minimum of an empty foldable."
  | otherwise = foldl1 min l
maxList'::(Ord a) => [a] -> a
maxList' l
  | null l = error "Tried to find the maximum of an empty list."
  | otherwise = List.foldl1' max l
minList'::(Ord a) => [a] -> a
minList' l
  | null l = error "Tried to find the minimum of an empty list."
  | otherwise = List.foldl1' min l

isSorted::(Ord a) => [a]->Bool
isSorted [] = True
isSorted (x:[]) = True
isSorted (x:xs) = (x <= head xs) && isSorted xs

insertionSort::(Ord a)=>[a]->[a]
insertionSort [] = []
insertionSort lst = List.foldl' insertInOrder [] lst
    where 
      insertInOrder [] element = [element]
      insertInOrder acc element = if element <= (head acc)
        then element:acc
        else (head acc):(insertInOrder (tail acc) element)

quickSort::(Ord a)=>[a]->[a]
quickSort list = quickSort_core quickSort list
quickSort_core::(Ord a)=>([a]->[a])->[a]->[a]
--quickSort_core _ [] = [] --Arguably not needed since rec takes care of the base case.
quickSort_core rec (pivot:lst) = rec (filter (<= pivot) lst) ++ pivot:(rec $ filter (>pivot) lst) --the most naive approach; improve

quickerSort::(Ord a)=>[a]->[a]
quickerSort lst
    | null lst         = []
    | length lst <= 10 = insertionSort lst
    | otherwise        = quickSort_core quickerSort lst

insertionSort2::(a->a->Bool)->[a]->[a]
insertionSort2 _ [] = []
insertionSort2 rel lst = List.foldl' insertInOrder [] lst
    where 
      insertInOrder [] element = [element]
      insertInOrder acc element = if rel element (head acc)
        then element:acc
        else (head acc):(insertInOrder (tail acc) element)

perms::[a]->[[a]]
perms [] = [[]]
perms lst = perms_helper [] lst
perms_helper::[a]->[a]->[[a]]
perms_helper front [] = []
perms_helper front back = (map ((head back):) (perms (front ++ tail back))) ++ (perms_helper ((head back):front) (tail back))
--Possibly the messiest function in this file.

powerSet::[a]->[[a]]
powerSet [] = [[]]
powerSet lst = map (head lst:) (powerSet . tail $ lst) ++ powerSet (tail lst)

cartesianProduct::[a]->[b]->[(a,b)]
cartesianProduct a b = [(aElem, bElem) | aElem <- a, bElem <- b]

count::(Integral b) => (a->Bool) -> [a] -> b
count condition = countTrue . map condition --TODO write as own function (more efficient?)

countTrue::(Integral a) => [Bool] -> a
countTrue = List.foldl' (\acc->(\e->if e then succ acc else acc)) 0

makeCounts::(Ord a, Foldable t, Integral b) => t a -> Map.Map a b
makeCounts = foldl addCount Map.empty where
    addCount acc e = 
        if Map.member e acc
            then Map.update (Just . succ) e acc
            else Map.insert e 1 acc

x <-$ f = f x
infixl 0 <-$

f <<= m = m >>= f
infixr 1 <<=
    
