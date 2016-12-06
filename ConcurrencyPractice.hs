import Control.Concurrent

oneSecond = 1000000

delayedProducer delay chan n 
    | n==0      = (writeChan chan n)
    | otherwise = (writeChan chan n) >> threadDelay delay >> yield >> delayedProducer delay chan (n-1)

decisecondwiseProducer = delayedProducer (div oneSecond 10)
secondwiseProducer = delayedProducer oneSecond

readAndPrint chan = do
    myInt <- readChan chan
    putStr "Read value: "
    print myInt
    yield
    if myInt/=0
      then readAndPrint chan
      else return () -- When I read a 0, exit.

main = do
    chan <- newChan
    forkIO $ decisecondwiseProducer chan 200
    forkIO $ secondwiseProducer chan 100
    forkIO $ secondwiseProducer chan 55
    forkIO $ secondwiseProducer chan 25
    readAndPrint chan