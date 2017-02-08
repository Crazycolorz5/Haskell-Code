module Main where
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    putStr "MESSAGE "
    putStrLn (head args)
