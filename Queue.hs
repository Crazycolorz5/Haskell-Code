--Crazycolorz5@gmail.com
module ColorzQueue where
import System.Environment
import Data.List


class Queue a where
    enqueue::a v->v->a v
    dequeue::a v->(v, a v)
    dequeue q = (front q, back q)
    front::a v->v
    back::a v->a v
    empty::a v
    singleton::v->a v
    isEmpty::a v->Bool
    isSingleton::a v->Bool
    queueLength::a v->Int
    toList::a v->[v]
    fromList::[v]->a v
    fromList = foldl (\acc->(\e->enqueue acc e)) empty
    --shorterThan::(Integral b)=>a v->b->Bool

{-
instance Queue [] where
    enqueue [] v = v:[]
    enqueue (h:t) v = h:((enqueue t) $! v)
    --enqueue lst v = listEnqueue_helper lst v []
    --Suspensions are built up in constant time enqueues until you want a dequeue;
    --Then they are evaluated at once, and a list in order is built up in correct (reverse) order.
    --Amortized constant time.
    
    --This is implicitly equivalent to the two stacks approach to implementing a queue.
    --http://stackoverflow.com/a/1740603
    
    front [] = error "Front of an empty queue."
    front (h:t) = h
    
    back [] = error "Back of an empty queue."
    back (h:t) = t
    
    emptyQueue = []
    isEmpty = null
    isSingleton (a:[]) = True
    isSingleton _ = False
    length = Prelude.length
-}

data TwoStacks a = TwoStacks [a] [a] Int --Cache length
instance Queue TwoStacks where
    enqueue (TwoStacks left right len) v = TwoStacks (v:left) right (len+1)
    
    front (TwoStacks [] [] len) = error "Tried to take the front of an empty queue."
    front (TwoStacks left [] len) = head . reverse $ left
    front (TwoStacks left right len) = head right
    
    back (TwoStacks [] [] len) = error "Tried to take the back of an empty queue."
    back (TwoStacks left [] len) = TwoStacks [] (reverse . init $ left) (len - 1)
    back (TwoStacks left right len) = TwoStacks left (tail right) (len - 1)
    
    dequeue (TwoStacks [] [] len) = error "Tried to dequeue an empty queue."
    dequeue (TwoStacks left [] len) = let revEnq = reverse left in
        (head (revEnq), TwoStacks [] (tail revEnq) (len - 1))
        
    empty = TwoStacks [] [] 0
    
    singleton a = TwoStacks [] (a:[]) 1
    
    isEmpty (TwoStacks [] [] len) = True
    isEmpty _ = False
    
    isSingleton (TwoStacks a b len) = len == 1
    --isSingleton (TwoStacks [] (a:[])) = True
    --isSingleton _ = False
    
    {-
    shorterThan (TwoStacks [] []) _ = False
    shorterThan (TwoStacks (a:[]) []) 0 = True
    shorterThan (TwoStacks [] (a:[])) 0 = True
    shorterThan (TwoStacks (a:t) right) n = shorterThan (TwoStacks t right) (pred n)
    shorterThan (TwoStacks [] (a:t)) n = shorterThan (TwoStacks [] t) (pred n)
    -}
    
    queueLength (TwoStacks left right len) = len
    
    toList (TwoStacks left right len) = right ++ reverse left


testMakeQ::Int -> TwoStacks Int
testMakeQ n = (foldl' enqueue emptyQueue [0..n])
testDeQ::TwoStacks Int -> Int
testDeQ q
    | isSingleton q = front q
    | otherwise = testDeQ $ back q
testQ::Int -> Int
testQ n = testDeQ (testMakeQ n)


{-
testQ results for list queue

1000 - (0.27 secs, 80,570,024 bytes)
2000 - (1.02 secs, 334,390,920 bytes)
3000 - (2.48 secs, 770,814,504 bytes)
4000 - (3.99 secs, 1,382,447,792 bytes)
5000 - (7.37 secs, 2,174,516,368 bytes)
10000 - (40.10 secs, 8,809,475,632 bytes)


Compiled
1000 - 
real	0m0.036s
user	0m0.023s
sys	0m0.010s

2000 - 
real	0m0.112s
user	0m0.091s
sys	0m0.018s

3000 - 
real	0m0.234s
user	0m0.203s
sys	0m0.027s

4000 - 
real	0m0.420s
user	0m0.379s
sys	0m0.036s

5000 - 
real	0m0.704s
user	0m0.640s
sys	0m0.049s

10000 - 
real	0m3.105s
user	0m2.980s
sys	0m0.114s
-}
--main::IO ()
main = getArgs >>= \x->print . testQ . read . head $ x

--a = enqueue emptyQueue 0::[Int]
--b = enqueue a 1
--c = enqueue b 2
--d = enqueue c 3
--e = enqueue d 4
