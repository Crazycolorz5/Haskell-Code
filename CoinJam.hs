import MyUsefulFunctions (primes, isPrime)
import Data.Bits
import Data.Word
import System.IO

type JamcoinType = Word16


asBase::JamcoinType->Int->Int
asBase l b = asBase_helper l b 0
asBase_helper::Word16->Int->Int->Int
asBase_helper 0 _ acc = acc
asBase_helper x b acc = asBase_helper (shiftR x 1) b (if (testBit x 0) then b*acc+1 else b*acc)

compositeProof::(Integral a)=>a->a
compositeProof n = head . filter (\p->mod n p == 0) $ primes

isJamcoin::JamcoinType->Bool
isJamcoin coin = testBit coin 0 {- && testBit coin (n - 1)-} && all (not . isPrime . asBase coin) [2..10]

toBinString::JamcoinType->String
toBinString 0 = []
toBinString b = (if testBit b 0 then '1' else '0'):toBinString (shiftR b 1)

main = output >>= \handle->
    (printCase handle >> let jamcoins = take j $ (filter (isJamcoin . fromIntegral) [firstCoin, firstCoin+2..shiftL 1 n - 1]::[Word16]) in   mapM_ (hPutStrLn handle . outputJamcoin) $ 
    do 
        aCoin <- jamcoins
        let baseExpressions = map (asBase aCoin) [2..10]
        return (toBinString aCoin, (map (fromIntegral . compositeProof) baseExpressions))
    )
  where
    firstCoin = (shiftL 1 (pred n) + 1)

printCase handle = hPutStrLn handle "Case #1:"
n::Int
n = 16
j::Int
j = 50

outputJamcoin::(String, [Int])->String
outputJamcoin (rep, proof) = rep ++ " " ++ spaceSep proof

spaceSep [] = []
spaceSep (x:[]) = show x
spaceSep (x:xs) = show x ++ ' ':spaceSep xs