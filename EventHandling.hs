import Control.Concurrent

type Process a b = (a->b)->[b->IO()]->IO ()

data ProcessData a b = ProcessData (a->b) [b->IO()]

executeProcess::ProcessData a b -> a -> IO()
executeProcess (ProcessData f dependencies) a = mapM_ (forkIO . ($ (f a))) dependencies

myDependencies = [print, printDouble, printTriple, printQuad, printQuint]

myProc = ProcessData id myDependencies

main = executeProcess myProc 1


printN n x = print (n*x)
printDouble = printN 2
printTriple = printN 3
printQuad = printN 4
printQuint = printN 5