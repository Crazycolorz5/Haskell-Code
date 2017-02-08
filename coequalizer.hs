import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)

makeCongruenceClasses :: (Eq a, Hashable a) =>  [(a, a)] -> [a] -> HashMap a a
makeCongruenceClasses l representatives = foldl linkOrInsert (selfMap representatives) l where
    selfMap = foldl (\acc e->Map.insert e e acc) Map.empty
    linkOrInsert hmap (a1, a2) = case Map.lookup a1 hmap of
                                      Just conclass -> Map.insert a2 conclass hmap
                                      Nothing -> case Map.lookup a2 hmap of
                                                      Just conclass -> Map.insert a1 conclass hmap
                                                      Nothing -> Map.insert a2 a1 (Map.insert a1 a1 hmap)
                                                      
coequalize :: (Eq a, Hashable a) => (a -> b) -> [(a, a)] -> [a] -> (a -> b)
coequalize f relation reps = f . \a -> Map.lookupDefault a a (makeCongruenceClasses relation reps)
--Requirement: f is defined on reps and the union of reps and all members of congruence classes not given a representative.

relation = [(2,1), (3,1), (4,5)]
representatives = [1, 4]

f :: Int -> Char
f 1 = 'x'
f 4 = 'y'

coeqF = coequalize f relation representatives
