import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array    -- for printing the value of a bad array
import Foreign.Marshal.Alloc    -- for free
import Foreign.Storable
-- import Foreign.C.Types       -- Alternately, if I want to use CULLong and CLLong
import Data.Int                 -- Int64 (long)
import Data.Word                -- Word64 (unsigned long)

foreign import ccall range::Ptr Word64->IO (Ptr Int64)
foreign import ccall shuffle::Ptr Int64->Ptr Word64->IO ()
foreign import ccall quickSort::Ptr Int64->Ptr Word64->IO ()

foreign import ccall srand::Int -> IO ()
foreign import ccall time::Int -> IO Int
  
isSorted lst = and $ zipWith (>=) (tail lst) lst

makeRef::(Storable a)=>a -> IO(ForeignPtr a)
makeRef long = do 
    ptr <- mallocForeignPtr
    withForeignPtr ptr $ flip poke long 
    return ptr

main = time 0 >>= srand {- >> putStrLn "initialized randomizer" -} >> (sequence . take 100 . repeat $ 
    let arrayLen = 100 in do
        arrayLenRef <- makeRef arrayLen
        
        myArray <- withForeignPtr arrayLenRef $ (newForeignPtr finalizerFree =<<) . range --(\len->(range len) >>= newForeignPtr finalizerFree)
        -- if I wasn't deallocating this array at the rnd of this routine, I'd consider using a ForeignPtr.
        --putStrLn "range array'd"
    
        withForeignPtr myArray $ withForeignPtr arrayLenRef . shuffle
        --putStrLn "array shuffle'd"
        --peekArray (fromIntegral arrayLen) myArray >>= print
    
        withForeignPtr myArray $ withForeignPtr arrayLenRef . quickSort
        --putStrLn "array sort'd"
    
        -- output array after marshalling
        haskArr <- withForeignPtr myArray $ \arr -> (withForeignPtr arrayLenRef $ \len -> peekArray (fromIntegral arrayLen) arr)
    
        if isSorted haskArr
            then putStrLn "Array properly sorted!"
            else putStr "Bad sort! " >> print haskArr
    
        -- dealloc array
        -- free $ castPtr myArray
    )
