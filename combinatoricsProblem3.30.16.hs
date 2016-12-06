import MyUsefulFunctions

hasMatch::(Eq a)=>[(a,a)]->Bool
hasMatch = or . (map (\a->fst a == snd a))
bruteForce::(Integral a)=>a->a
bruteForce n = (factorial n) - numMatches
  where numMatches = countTrue $ map (\aPerm->hasMatch $ zip [1..n] aPerm) $ perms [1..n]
  
ppBruteForce n = putStr (show numerator) >> putStr "/" >> putStrLn (show denominator) >> print (fromIntegral (numerator) / fromIntegral (denominator)) 
  where 
      numerator = bruteForce n
      denominator = factorial n