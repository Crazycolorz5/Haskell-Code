{- 
Doing all this in 4 hours to see how far I can get into the list.
Restrictions: All problems names must be "problemN" where N is the number.
The only top-level expressions are the problems
The standard library functions I'm allowed to use are:
    map, foldl, foldr, filter
    fst, snd
    All operators (+) (.) etc
    Arithmetic operators (mod/rem/div)
    functions on functions (flip, fix)
    Things otherwise not implementable/feasible to implement (e.g. randomization)
Ok? Let's jam:
-}

import System.Random

--Start time: 1:51 PM Jan 6 2017
problem1 (x:[]) = x
problem1 (x:xs) = problem1 xs

problem2 (x:y:[]) = x
problem2 (x:xs) = problem2 xs

problem3 (x:xs) 0 = x
problem3 (x:xs) n = problem3 xs (n-1)

problem4 :: [a] -> Int
problem4 = foldl (\acc e->acc+1) 0

problem5 :: [a] -> [a]
problem5 = foldl (flip (:)) []

problem6 l = l == problem5 l

data NestedList a = Elem a | List [NestedList a]
problem7 (Elem a) = [a]
problem7 (List l) = l >>= problem7

problem8 [] = []
problem8 (x:xs) = x : inner x xs where
    inner prev [] = []
    inner prev (x:xs) = if x == prev then inner prev xs else x : inner x xs

problem9 [] = []
problem9 (x:xs) = inner x (x:xs) [] where
    inner _ [] acc = [acc]
    inner cur (x:xs) acc = if cur == x then inner cur xs (x:acc) else acc : inner x (x:xs) []
    
problem10::(Eq a) => [a]->[(Int,a)]
problem10 = map (\l@(x:xs)->(length l, x)) . problem9

data ListElem a = Single a | Multiple Int a deriving Show
problem11::(Eq a) => [a]->[ListElem a]
problem11 = map (\l -> case l of (x:[]) -> Single x; (x:xs) -> Multiple (length l) x) . problem9
                            
problem12::(Eq a) => [ListElem a] -> [a]
problem12 = foldl (\acc e->acc++ case e of Single a -> [a]; Multiple n a -> rep n a) [] where
    rep 0 a = []
    rep n a = a:(rep (n-1) a)

problem13 [] = []
problem13 (x:xs) = inner x (x:xs) 0 where
    inner cur [] 1 = [Single cur]
    inner cur [] acc = [Multiple acc cur]
    inner cur (x:xs) acc = if cur == x 
                              then inner cur xs (acc+1) 
                              else if acc==1
                              then Single cur : inner x xs 1
                              else Multiple acc cur : inner x xs 1
                              
problem14 l = l >>= (\e->[e,e])

problem15 l n = l >>= rep n where 
    rep 0 a = []
    rep n a = a:(rep (n-1) a)
    
problem16 l n = inner l n (n-1) where
    inner [] _ _ = []
    inner (x:xs) n 0 = inner xs n (n-1)
    inner (x:xs) n m = x : inner xs n (m-1)
    
problem17 l n = inner l n [] where
    inner [] n acc = (problem5 acc, [])
    inner l 0 acc = (problem5 acc, l)
    inner (x:xs) n acc = inner xs (n-1) (x:acc)
    
problem18 l sta end = ans where
    (_, a) = problem17 l sta
    (ans, _) = problem17 a (end-sta+1)

problem19 l n = b ++ a where
    (a,b) = problem17 l (mod n (problem4 l))
    
problem20 (x:xs) 1 = (x, xs)
problem20 (x:xs) n = let (a,b) = problem20 xs (n-1) in (a, x:b)
                         
--3:26 remaining

problem21 x [] _ = [x]
problem21 e (x:xs) 1 = e:x:xs
problem21 e (x:xs) n = x : problem21 e xs (n-1)

problem22 n m = problem18 [0..] n m

problem23 :: [a] -> Int -> IO [a]
problem23 _ 0 = return []
problem23 [] _ = return []
problem23 l n = let len = problem4 l in do 
    i <- getStdRandom (randomR (1,len))
    let (e, ls) = problem20 l i
    rec <- problem23 ls (n-1)
    return (e:rec)

problem24 n m = sequence $ map (const (getStdRandom $ randomR (1,m))) [1..n]

problem25 l = problem23 l (problem4 l)

problem26 0 _ = [[]]
problem26 n l = if n > problem4 l then [] else if n == problem4 l then [l] else let (x:xs)=l in map (x:) (problem26 (n-1) xs) ++ problem26 n xs

problem27a :: (Eq a) => [a] -> [[[a]]]
problem27a = problem27b [2,3,4]

problem27b :: (Eq a) => [Int] -> [a] -> [[[a]]]
problem27b (x:[]) list = [[list]]
problem27b (x:xs) list = do 
    chosen <- problem26 x list 
    let unchosen = filter (not . flip elem chosen) list
    otherGroups <- problem27b xs unchosen 
    return $ chosen:otherGroups
    
problem28a l = let sort [] cmp = []; sort (x:xs) cmp = sort (filter (cmp x) xs) cmp ++ x:sort (filter (not . cmp x) xs) cmp in
        sort l (\e1 e2 -> problem4 e1 >= problem4 e2)
problem28b :: (Ord a) => [[a]] -> [[a]]
problem28b l = let 
                    sort [] cmp = []; sort (x:xs) cmp = sort (filter (cmp x) xs) cmp ++ x:sort (filter (not . cmp x) xs) cmp
                    inOrder = sort (map problem4 l) (<=) --Sort list to be able to use run-length encoding
                    lengths = sort (map fst . problem10 $ inOrder) (<=) --Construct a sorted list of frequencies
                    inOrderLengths = map snd $ sort (problem10 lengths) (\(a,b) (c,d) -> a >= c) --Encode the frequencies once again and sort by freq of freq
                    cmp a b = cmpInner (problem4 a) (problem4 b) inOrderLengths --finally, a <= b if a's length appears in the list before b's length
                    cmpInner a b (x:xs) = if a == x then True else if b == x then False else cmpInner a b xs
               in sort l cmp

-- 2:28 left

problem31 x = let primes = 2:3:filter problem31 [5..] in
            not . any ((==0) . mod x) $ takeWhile (< div x 2) primes

problem32 a 0 = a
problem32 a b = if a < b then problem32 b a else problem32 b (mod a b)

problem33 = curry $ fmap (==1) (uncurry problem32)

problem34 n = problem4 . filter (problem33 n) $ [1..n]

problem35 1 = []
problem35 n = let firstThat p (x:xs) = if p x then x else firstThat p xs
                  primes = 2:3:filter problem31 [5..]
                  smallestPrimeDivisor = firstThat ((==0) . mod n) primes
              in smallestPrimeDivisor : problem35 (div n smallestPrimeDivisor)

problem36 = map (\(a,b)->(b,a)) . problem10 . problem35

problem37 = foldl (*) 1 . map (\(a,b) -> (a-1)*a^(b-1)) . problem36

problem39 low high = let primes = 2:3:filter problem31 [5..] in
        takeWhile (<=high) . dropWhile (<low) $ primes

problem40 n = let firstThat p (x:xs) = if p x then x else firstThat p xs 
                  primes = problem39 2 n
                  x = firstThat (flip elem primes . (n-)) $ primes
              in (x, n-x)
              
problem41a l h = map problem40 $ filter ((==0) . flip mod 2) [l..h]
problem41b l h lim = filter ((>50) . fst) $ map problem40 $ filter ((==0) . flip mod 2) [l..h]

-- 2:01 left
and' = (&&)
or' = (||)
nand = fmap not . (&&)
nor = fmap not . (||)
xor :: Bool -> Bool -> Bool
xor = fmap not . (==)
impl a b = not a || a && b
equ::Bool->Bool->Bool
equ = (==)
problem46::(Bool->Bool->Bool) -> [(Bool, Bool, Bool)]
problem46 f = do
    a <- [True, False]
    b <- [True, False]
    return (a, b, f a b)
    
-- problem47
infixl 4 `and'`
infixl 3 `or'`

problem48::Int->([Bool]->Bool) -> [([Bool],Bool)]
problem48 n f = let argl 0 = [[]]; argl n = (fmap (True:) $ argl (n-1)) ++ (fmap (False:) $ argl (n-1)) in do
    args <- argl n
    return (args, f args)

--1:46 remaining
problem49
