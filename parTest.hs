import Control.Parallel.Strategies

n = 100000000

main = print . sum . withStrategy (parListChunk n r0) $ map (*2) [1..100000000] 
