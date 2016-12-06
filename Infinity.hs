module Infinity where

infinity 1 0 0 = 1
infinity _ 0 0 = 0
infinity 0 1 0 = 1
infinity 0 _ 0 = 0
infinity 0 0 1 = 1
infinity 0 0 _ = 0
infinity a b c
  | a == 0 = ((2^c)-1) + infinity a (b-1) c --b /= 0 and c /= 0 or else pattern would have matched to base case.
  | b == 0 = c + infinity (a-1) b c
  | otherwise = (2^(b+c)-1) * infinity (a-1) b c 
  
  
  
{-
infinity 0 1 _ = 1
infinity 0 2 2 = 5
infinity 1 1 1 = 3
infinity 0 2 3 = 20 (2(symmetry)*((2^3-1)*infinity 0 1 3)) + 6(new cases)
-}