type Word a = [a]
e = []::(Word a)

type Polynomial a = [Word a]


class MaybeOrd a where
    (<)::(MaybeOrd a)=>a->a->(Maybe Bool)
    (<) a b = (a > b)>>=(Just . not)
    (>)::(MaybeOrd a)=>a->a->(Maybe Bool)
    (<) a b = (a < b)>>=(Just . not)
    (==)::(MaybeOrd a)=>a->a->(Maybe Bool)
    --(>=) = 
--Nothing value represents bot being able to be compared.



instance MaybeOrd (Word a) where
    (<) one two = 


polynomial u v w
    | (w < (inverse u) * v) == (Just True) || (w < (inverse u) * v) == Nothing
        = [0]
    | (w == (inverse u) * v) == (Just True)
        = [1]
    | otherwise
        = let s = last u
          in  if (s*w > w) == (Just True) 
                  then polynomial (u*s) v (s*w)
                  else map (\e->((fst e) + (snd e))) $ zip_with_extend (polynomial (u*s) v (s*w)) $ 0:(polynomial (u*s) v w)

inverse = reverse

zip_with_extend [] [] = []
zip_with_extend [] (b:br) = (0,b):(zip_with_extend [] br)
zip_with_extend (a:ar) [] = (a,0):(zip_with_extend ar [])
zip_with_extend (a:ar) (b:br) = (a,b):(zip_with_extend ar br)