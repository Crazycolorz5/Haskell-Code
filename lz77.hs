import Prelude hiding (length, take, drop)
import Data.Sequence
import Data.Foldable (toList)
import qualified Data.ByteString.Lazy as Bytes
import Data.Word (Word8)
import Data.Binary
type Lz77Data a = Seq (Word16, Word16, Maybe a)

testStr::String
testStr = "aacaacabcabaaac"


prettyPrint::(Show a)=>Lz77Data a -> IO ()
prettyPrint  = mapM_ (\dat-> case dat of
    (a, b, Just c) -> print (a,b,c)
    (a, b, Nothing) -> print (a, b, '$'))


-- data LZData a = Literal a (Maybe a) (Maybe a) | Repeat Int Int (Maybe a)
-- Might be worth adding this later.

lz77Compress::(Eq a) => Word16 -> [a] -> Lz77Data a
lz77Compress windowSize lst = lz77Compress_main windowSize (fromList lst) 0

lz77Compress_main::(Eq a) => Word16 -> Seq a -> Int -> Lz77Data a
lz77Compress_main windowSize lst offset
    | offset == 0           = (0,0,Just $ index lst 0) <| lz77Compress_main windowSize lst (offset+1)
    -- | offset >= length lst  = return ()
    | otherwise   = if nextIndex < fromIntegral (length lst)
        then (lookbehind, seqLength, Just nextElem) <| lz77Compress_main windowSize lst (nextIndex+1)
        else singleton (lookbehind, seqLength, Nothing)
      where
        windowStart::Int
        windowStart = max 0 (offset - fromIntegral windowSize)
        windowEnd::Int
        windowEnd = offset - 1
        indexOfMaxMatch::Int
        indexOfMaxMatch = (getIndexOfMax . fmap (fromIntegral . matchLength lst offset) $ fromList [windowStart..windowEnd]) + windowStart
        seqLength::Word16
        seqLength = fromIntegral $ matchLength lst offset indexOfMaxMatch
        lookbehind::Word16
        lookbehind = if seqLength > 0 then fromIntegral (offset-indexOfMaxMatch) else 0
        nextIndex::Int
        nextIndex = offset+fromIntegral seqLength
        nextElem = index lst nextIndex

lz77Decompress::(Eq a) => Lz77Data a -> [a]
lz77Decompress lzData = lz77Decompress_main lzData 0 empty

--We know what length array we would need to hold the data:
--sizeof(a) * ((sum . map snd $ lzData) + length lzData - 1)
lz77Decompress_main::(Eq a) => Lz77Data a -> Int -> Seq a -> [a]
lz77Decompress_main lzData offset acc = let (lookBack, seqLength, terminator) = index lzData offset in
      case terminator of
          Just terminatingElem -> let newAcc = acc >< (take (fromIntegral seqLength) (drop (length acc-fromIntegral lookBack) acc) |> terminatingElem) in
              lz77Decompress_main lzData (succ offset) newAcc
          Nothing              -> toList $ acc >< take (fromIntegral seqLength) (drop (length acc-fromIntegral lookBack) acc)
        

getIndexOfMax::(Ord a)=>Seq a->Int
getIndexOfMax lst = snd . foldl (\acc->(\e->if e > fst acc then (e, trd acc, trd acc + 1) else (fst acc, snd acc, trd acc + 1))) (index lst 0, 0::Int, 0::Int) $ lst
    where
      fst (a,b,c) = a
      snd (a,b,c) = b
      trd (a,b,c) = c

matchLength::(Eq a, Integral b)=>Seq a->b->b->b
matchLength lst off1 off2 = 
    if fromIntegral off1 >= length lst || fromIntegral off2 >= length lst || index lst (fromIntegral off1) /= index lst (fromIntegral off2)
      then 0
      else 1 + matchLength lst (succ off1) (succ off2)

--TODO: Read a lazy ByteString from a file, lz77 compress it, and write the output, being able to factor out the literal-ness of an entry for 8 entries at a time.
slidingWindowSize::Word16
slidingWindowSize = 1024

lz77CompressFile::String -> IO (Lz77Data Word8)
lz77CompressFile = fmap (lz77Compress slidingWindowSize . Bytes.unpack) . Bytes.readFile

lz77DataToByteString::Lz77Data Word8 -> Bytes.ByteString
lz77DataToByteString dat = Bytes.append (encode $ length dat) (foldr bytifyEntry Bytes.empty dat) where
    bytifyEntry (lookBehind, seqLength, Nothing) acc       = Bytes.append (encode lookBehind) $ Bytes.append (encode seqLength) (Bytes.cons (0::Word8) acc)
    bytifyEntry (lookBehind, seqLength, Just terminal) acc = Bytes.append (encode lookBehind) $ Bytes.append (encode seqLength) (Bytes.cons terminal acc)
