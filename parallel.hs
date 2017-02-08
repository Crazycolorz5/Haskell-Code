module Main where

import Control.Parallel.Strategies
    (Strategy,withStrategy,parListChunk,rseq)
import Data.List (foldl')
import Data.Time.Clock
import System.Environment

parMapChunked
  :: Strategy b -- ^ evaluation degree at each element
  -> Int        -- ^ chunk size
  -> (a -> b)   -- ^ function to apply to each element
  -> [a]        -- ^ input list
  -> [b]
parMapChunked strat i f =
    withStrategy (parListChunk i strat) . map f

addFive = collatzLength --(+5)

main = do
    args <- getArgs
    
    let length    = read $ args!!0
        chunkSize = read $ args!!1

        -- dynamic chunked parallelism
        newList = parMapChunked rseq chunkSize addFive [1..length]

        -- accumulate the result
        sum = foldl' (+) 0 newList
        
        newListSingleThreaded = map addFive [1..length]
        sumSingleThreaded = foldl' (+) 0 newListSingleThreaded
        
    threadedStartTime <- getCurrentTime    
    print sum
    threadedEndTime <- getCurrentTime
    putStrLn ("Threaded time taken: " ++ show (diffUTCTime threadedEndTime threadedStartTime))
    
    startTime <- getCurrentTime    
    print sumSingleThreaded
    endTime <- getCurrentTime
    putStrLn ("Single-thread time taken: " ++ show (diffUTCTime endTime startTime))

collatzLength :: (Integral a) => a -> Int
collatzLength n = if n==1 then 0 else 1 + collatzLength (if even n then div n 2 else (n*3+1))
