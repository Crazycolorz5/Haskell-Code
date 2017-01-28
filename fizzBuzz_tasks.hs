{-
    Reverse a string
    Reverse a sentence ("bob likes dogs" -> "dogs likes bob")
    Find the minimum value in a list
    Find the maximum value in a list
    Calculate a remainder (given a numerator and denominator)
    Return distinct values from a list including duplicates (i.e. "1 3 5 3 7 3 1 1 5" -> "1 3 5 7")
    Return distinct values and their counts (i.e. the list above becomes "1(3) 3(3) 5(2) 7(1)")
    Given a string of expressions (only variables, +, and -) and a set of variable/value pairs (i.e. a=1, b=7, c=3, d=14) return the result of the expression ("a + b+c -d" would be -3).
-}

problem_one = reverse

problem_two = unwords . reverse . words

problem_three::(Ord a)=>[a]->a
problem_three = foldl1 min

problem_four::(Ord a)=>[a]->a
problem_four = foldl1 max

problem_five = rem

problem_six::(Eq a)=>[a]->[a]
problem_six = foldl (\acc e->(if elem e acc then acc else e:acc)) []

{-
problem_seven::(Eq a)=>[a]->[a]
problem_seven lst = let 
    count::(Eq a)=>a->[a]->Int
    count elem = length . filter (==elem)
    add_counts::[(a, Int)]
    add_counts = map (\x->(x, count x lst)) lst
    in problem_six add_counts
-}

a = take 99 [2..]
b = a
c = a >>= \x-> map ($x) $ map (^) b
main = print $ length $ problem_six c