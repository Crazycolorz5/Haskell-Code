{-# LANGUAGE BangPatterns #-}

import Control.Parallel
import Control.Parallel.Strategies
import Data.Matrix
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.CPUTime
import Data.Time.Clock
import System.Environment
import Control.Monad

parVector :: Strategy a -> Strategy (Vector a)
parVector = parTraversable--sequence $ V.map (\e -> strat e) vect

--evalVector :: Strategy a -> Strategy (Vector a)


parVectorChunk :: Int -> Strategy a -> Strategy (Vector a)
parVectorChunk n strat vect = fmap join $ parTraversable (evalTraversable strat) (chunkVector n vect)

parVectorMapChunked :: Strategy b -> Int -> (a -> b) -> Vector a -> Vector b
parVectorMapChunked strat i f = withStrategy (parVectorChunk i strat) . V.map f

chunkVector :: Int -> Vector a -> Vector (Vector a)
chunkVector n xs = let len = div len n + signum (mod len n)
                   in V.generate n (\i -> V.take n (V.drop (i*n) xs)) 
{-
chunkVector _ empty = []
chunkVector n xs = cons as $ chunkVector bs where (as, bs) = V.splitAt n xs
-}

{- Constraint: a is an NxM matrix, b is MxP. Output: NxP matrix -}
multiplyMatricesPar::(Num a, NFData a) => Int -> Matrix a -> Matrix a -> Matrix a
multiplyMatricesPar chunkSize a b = c where
    n = nrows a
    m = nrows b -- = ncols a
    p = ncols b
    {-rowPartitionBounds = map ((flip div numThreads) . (n*)) $ take (numThreads+1) [0..]
    rowPartitionTuple = zip rowPartitionBounds (tail rowPartitionBounds)
    rowPartition = map (\(a,b) -> [a..b-1]) rowPartitionTuple-}
    cSplit = parVectorMapChunked rseq (chunkSize) (calcRow a b) (V.generate n id)
    --cList = concat cSplit
    c = matrix n p (\(i,j) -> (cSplit V.! (pred i)) V.! (pred j))
   
calcRow matA matB n =   let cols = V.map (flip getCol matB) $ V.generate (ncols matB) succ
                            row = getRow (n+1) matA in
                        V.map (V.sum . V.zipWith (*) row) cols 
    
a = matrix 1000 800 (\(a,b)->a+b)
b = matrix 800 600 (\(a,b)->a*b)

test :: Int -> Int -> Int -> Int -> IO ()
test chunkSize n m p = do
    startTime <- getCurrentTime
    print . sum . getCol 1 $ (multiplyMatricesPar chunkSize (matrix n m (\(a,b)->a+b)) (matrix m p (\(a,b)->a*b)))
    endTime <- getCurrentTime
    putStrLn ("Time taken with " ++ show (div n chunkSize) ++ " threads: " ++ show (diffUTCTime endTime startTime))
    
collatzLength :: (Integral a) => a -> Int
collatzLength (!n) = if n==1 then 0 else 1 + collatzLength (if even n then div n 2 else (n*3+1))
    
parMapChunked
  :: Strategy b -- ^ evaluation degree at each element
  -> Int        -- ^ chunk size
  -> (a -> b)   -- ^ function to apply to each element
  -> [a]        -- ^ input list
  -> [b]
parMapChunked strat i f =
    withStrategy (parListChunk i strat) . map f
    
testPar n chunk = do
    {-
    let bounds = map ((flip div numThreads) . (n*)) $ take (numThreads+1) [0..]
    let partition = map (\(a,b) -> [a..b-1]) (zip bounds (tail bounds))
    res1 <- seq partition (return ())-}
    startTime <- getCurrentTime
    print $ sum $ (parVectorMapChunked rseq chunk collatzLength $ V.generate n succ) --seq (concat $ parMap rdeepseq (map (*2)) partition) (return $ concat $ parMap rdeepseq (map (*2)) partition)
    endTime <- getCurrentTime
    putStrLn ("Threaded time taken: " ++ show (diffUTCTime endTime startTime))
    
    singleStart <- getCurrentTime
    print $ sum $ map collatzLength [1..n]
    singleEnd <- getCurrentTime
    putStrLn ("Single-thread time taken: " ++ show (diffUTCTime singleEnd singleStart))
    
main = do
    args <- getArgs
    
    let n = read $ args!!0 
        m = read $ args!!1
        p = read $ args!!2
        chunkSize = read $ args!!3
        --length    = read $ args!!0
        --chunkSize = read $ args!!1
    
    --foldM (\acc val -> acc >> val) 
    test chunkSize n m p
    
    --testPar length chunkSize
    
main2 = do
    args <- getArgs
    
    let length    = read $ args!!0
        chunkSize = read $ args!!1 :: Int
        
    testPar length chunkSize
    
