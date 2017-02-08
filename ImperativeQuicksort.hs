import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array    -- for printing the value of a bad array
import Foreign.Marshal.Alloc    -- for free
import Foreign.Storable
import Data.Int                 -- Int64 (long)
import Data.Word                -- Word64 (unsigned long)

foreign import ccall rand::IO Int
foreign import ccall srand::Int -> IO ()
foreign import ccall time::Int -> IO Int

swap array i1 i2 = do
    temp <- peekElemOff array i1
    peekElemOff array i2 >>= pokeElemOff array i1
    pokeElemOff array i2 temp

quicksort array len = quicksort_rec array len 0 len

--quicksort::(Storable a, Ord a)=>Ptr a->Int->Int->Int->IO()
quicksort_rec array len low high = 
    if low+1 >= high
    then return ()
    else do
        pivot <- peekElemOff array low
        crossPoint <- qs_inner low pivot (low+1) (high-1)
        quicksort_rec array len low crossPoint
        quicksort_rec array len (crossPoint+1) high
            where
            qs_left pivot low high= do
                lowVal <- peekElemOff array low
                if  lowVal > pivot || low > high
                then return low
                else qs_left pivot (low+1) high
            qs_right pivot low high = do
                highVal <- peekElemOff array high
                if highVal < pivot || high < low
                then return high
                else qs_right pivot low (high-1)
            --qs_inner::Int->a->Int->Int->IO Int
            qs_inner pivotIndex pivot low high = 
                    if low > high
                    then swap array pivotIndex high >> return high
                    else do
                        lowSwap <- qs_left pivot low high
                        highSwap <- qs_right pivot lowSwap high
                        if lowSwap < highSwap
                            then swap array lowSwap highSwap >> qs_inner pivotIndex pivot lowSwap highSwap
                            else qs_inner pivotIndex pivot lowSwap highSwap

range::(Storable a, Integral a)=>Int->IO (Ptr a)
range n = do
    newArray <- mallocArray n
    mapM (\n->pokeElemOff newArray n $ fromIntegral n) $ take n [0..]
    return newArray

main::IO()
main = time 0 >>= srand >> (sequence . take 100 . repeat $ 
    let arrLen = 100 in do
    myArr <- range arrLen::IO(Ptr Int)
    shuffle myArr arrLen
    quicksort myArr $ fromIntegral arrLen
    haskellArray <- peekArray (fromIntegral arrLen) myArr
    --print haskellArray
    if isSorted haskellArray
        then putStrLn "Array sorted properly!"
        else putStrLn "Bad sort!"
    free myArr
    ) >> return ()
    
isSorted arr = and $ zipWith (<) arr (tail arr)

shuffle myArr len = mapM_ (\i-> rand >>= swap myArr i . (+i) . flip mod (len-i)) $ take len [0..]

