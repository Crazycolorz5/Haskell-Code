class Node a where
    children::(Node a)=>a->[a]

--data LatticePoint a b = LatticePoint a b deriving Show
type LatticePoint = (Int, Int)

instance Node Int where
    --children::(LatticePoint Int Int)->[LatticePoint Int Int]
    --children (LatticePoint a b) = [(LatticePoint (a+1) b), (LatticePoint (a-1) b), (LatticePoint a (b+1)), (LatticePoint a (b-1))]
    --children (a,b) = [(a+1,b), (a,b+1)]
    children a = (succ a):[]

instance (Num a, Num b)=>(Node (a,b)) where
    children (a,b) = [(a+1,b), (a,b+1), (a-1,b), (a,b-1)]
{-}
data TicTacToePoint = Empty | Player1 | Player2
type TicTacToeBoard = ((TicTacToePoint,TicTacToePoint,TicTacToePoint),(TicTacToePoint,TicTacToePoint,TicTacToePoint),(TicTacToePoint,TicTacToePoint,TicTacToePoint))


-}
