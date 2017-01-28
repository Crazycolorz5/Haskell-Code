import Control.Concurrent
import System.IO
import Data.Char

endl = putStrLn ""

main::IO ()
main = do
    putStrLn "Testing taskOne:"
    taskOne
    endl
    
    putStrLn "Testing taskTwo:"
    putStr "Length of \"Hello, world.\": "
    print $ taskTwo "Hello World"
    endl
    
    putStrLn "Testing taskThree:"
    putStr "Words in \"Hello, world.\": "
    print $ taskThree "Hello World"
    endl
    
    putStrLn "Testing taskFour:"
    taskFour 10
    endl
    
    putStrLn "Testing taskFive:"
    taskFive "success"
    taskFive "{error, secondInput}"
    endl
    
    putStrLn "Testing taskSix:"
    print $ taskSix [("erlang", "a functional language"), ("ruby", "an OO language")] "erlang"
    endl
    
    putStrLn "Testing taskSeven:"
    print $ taskSeven [("Eggs", 10, 1.5), ("Bread", 20, 2.5), ("Milk", 30, 3.5)]
    endl
    
    putStrLn "Testing taskEight:"
    taskEight 5
    endl
    
    putStrLn"Testing taskNine:"
    taskNine 5 3
    endl


taskOne::IO ()
taskOne = putStrLn "Hello, world."

taskTwo::String->Int
taskTwo [] = 0
taskTwo (x:xs) = 1 + taskTwo xs

taskThree::String->Int
taskThree lst = 
-- Number of words is equal to the number of nonempty strings that we get when we split by whitespace.
    length . --number of
    filter ((>0) . length) . --nonempty strings
    --split by whitespace
    foldl (\acc->(\e-> if isSeparator e then []:acc else (e:head acc):(tail acc))) [[]] $ lst

taskFour::Int->IO ()
taskFour limit = taskFour_rec limit 1
taskFour_rec limit cur = print cur >>
    -- if at limit, then just return (), else keep counting up.
    if limit==cur then return () else taskFour_rec limit (cur+1)

taskFive::String->IO()
taskFive "success" = putStrLn "success"
taskFive err = --extract message
    let message = init . tail . tail . dropWhile (/=',') $ err in
      --after the first comma, then discard the space and the brace.
      putStr "error: " >> putStrLn message

taskSix::(Eq k)=>[(k,v)]->k->v
taskSix dict key = head [v | (k,v)<-dict, k==key] --head to only get the first matching element.

taskSeven::(Integral q, Num a)=>[(i, q, a)]->[(i,a)]
taskSeven list = [(i, fromIntegral q*p) | (i,q,p) <- list]


-- Takes all inputs from inChan and forwards them to outChan.
-- Does this m times, then exits. Prints a message each time.
relay::(Integral a, Show a, Integral c)=>MVar c->Chan b->Chan b->a->a->IO ()
relay runningThreads inChan outChan m n
  --If we've done the action m times, terminate.
  | m==0      = decMVar runningThreads >> yield -- Decrement the count of active threads
  --Otherwise relay the value and print a message.
  | otherwise = do 
        value <- readChan inChan    -- Blocks until a value is available
        putStr "Received Message #" -- Print a message that you have received the value
        print n
        hFlush stdout               -- Because of Haskell's laziness, flush stdout to ensure it
                                    --   gets printed before another relay outputs their message.
        writeChan outChan value     -- Relay the value.
        relay runningThreads inChan outChan (m-1) (n+1) -- Continue doing this with 1 less use, and one more count

incMVar var = takeMVar var >>= putMVar var . (+1)
decMVar var = takeMVar var >>= putMVar var . (+ (-1))

-- Keeps the main thread alive while all spawned threads execute.
keepAlive::(Integral a, Show a)=>MVar a->IO()
keepAlive numThreads = takeMVar numThreads >>= \a->   --peek at number of other active threads
                        putMVar numThreads a >> 
                        if a==0 
                            then return ()            --if no more threads, terminate
                            else threadDelay 1000 >>  --else wait 1ms, then repeat
                                 keepAlive numThreads

taskEight::Int->IO ()
taskEight m = do
    chanA <- newChan       -- make a channel to go from a to b
    chanB <- newChan       -- and a channel to go from b to a
    runningThreads <- newMVar 0
    
    incMVar runningThreads
    forkIO $ relay runningThreads chanA chanB m 0 -- Make relay a
    
    incMVar runningThreads
    forkIO $ relay runningThreads chanB chanA m 0 -- Make relay b
    
    writeChan chanA True  --start the threads
    keepAlive runningThreads

taskNine::Int->Int->IO ()
taskNine n m = do
    -- make n new channels
    channels <- sequence $ take n $ repeat newChan 
    
    -- Pair consecutive channels up, with a loop from the end to the start.
    let conns = (last channels, head channels):(zip channels $ tail channels) 
    
    -- Initialize the count of active other threads
    runningThreads <- newMVar 0
    
    -- For each pair of channels, start a relay on them.
    mapM_ (\connection -> do 
        incMVar runningThreads
        forkIO $ relay runningThreads (fst connection) (snd connection) m 0) conns
    
    --start the relays
    writeChan (head channels) True 

    --wait to exit
    keepAlive runningThreads
