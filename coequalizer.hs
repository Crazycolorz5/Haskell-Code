import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.Set hiding (filter, foldl)
import Data.Maybe (fromJust, isNothing)

makeCongruenceClasses :: (Eq a, Hashable a) =>  [(a, a)] -> HashMap a Int
makeCongruenceClasses l = foldl linkOrInsert Map.empty l where
    linkOrInsert hmap (a1, a2) = case Map.lookup a1 hmap of
                                      Just conclass -> case Map.lookup a2 hmap of
                                                            Just class2 -> if conclass == class2
                                                                              then hmap
                                                                              else Map.map (\v-> if v==class2 then conclass else v) hmap-- we need to unify the classes
                                                            Nothing -> Map.insert a2 conclass hmap
                                      Nothing -> case Map.lookup a2 hmap of
                                                      Just conclass -> Map.insert a1 conclass hmap
                                                      Nothing -> let rep = (+1) . foldl max 1 . fromList $ Map.elems hmap in Map.insert a2 rep (Map.insert a1 rep hmap)
--TODO: I could make a congruenceClass with default, the default for an enumerable being its toEnum + the largest representitive of the cclass.                                                      
relation::[(Int, Int)]
relation = [(2,1), (3,4), (2,3), (5,6), (10,11)]
f x = let classes = makeCongruenceClasses relation in Map.lookupDefault (x + foldl max 0 (Map.keys classes)) x $ classes
--This generalizes to giving unique values to those not specified in the relation. This is a bit lost later --
                                                                     
--And note, the congruence classes created this way are ismorphic to a Hashmap a (Set a) by the following isomorphism:
isomorphism :: (Ord a, Hashable a) => HashMap a Int -> HashMap a (Set a)
isomorphism m = Map.foldlWithKey' (\acc k v -> Map.insert k (classTo v) acc) Map.empty m where
    classTo value = Map.foldlWithKey' (\acc k v -> if v==value then insert k acc else acc) empty m
--An inverse exists but is hard to construct programmatically (i.e. we'd have to preserve the index of each set, or something)
--It'd be easier if we picked [Set a] as the representation for the congruence classes, or something.


coequalize :: (Eq a, Hashable a) => HashMap a Int -> (a -> Maybe b) -> (Int -> b)
coequalize classes f i = fromJust (f firstElem) where firstElem = fst . head . filter (\e->snd e == i && (not . isNothing . f) (fst e)) . Map.toList $ classes

uncoequalize :: (Eq a, Hashable a) => HashMap a Int -> (Int -> b) -> (a -> b)
uncoequalize classes psi = let nu = fromJust . flip Map.lookup classes in psi . nu --Note: not an isomorphism

extendOverRelation :: (Eq a, Hashable a) => [(a, a)] -> (a -> Maybe b) -> (a -> b)
extendOverRelation rel f a = let cclasses = makeCongruenceClasses rel in if not . isNothing $ (f a) then fromJust (f a) else (uncoequalize cclasses . coequalize cclasses) f $ a
--The reason this works is that if f holds for anything in one of the congruence classes, then 
--eOR f will map to the first (defined value) in any class.
--If f has conflicting values for various members in the congruence class, behavior is not guaranteed.
--Defines as just the point value of f if it's not in a congruence class.
myF 2 = Just 'x'
myF 4 = Just 'y'
myF _ = Nothing

eORF = extendOverRelation relation myF

{-
import Data.Set hiding (foldl)
import Data.List (elemIndex)
import Data.Maybe

makeCongruenceClasses :: (Ord a) => [(a,a)] -> (a -> (Set a))
makeCongruenceClasses [] a = singleton a
makeCongruenceClasses ((a1, a2):as) a = let s = makeCongruenceClasses as a in
    if a1 `member` s
       then insert a2 s
       else if a2 `member` s
       then insert a1 s
       else s

coequalize :: (Ord a) => [(a, a)] -> (Set a -> b) -> (a -> b)
coequalize relation f a = let cclasses = makeCongruenceClasses relation in
                            f (cclasses a)
numberCongruenceClasses :: (Ord a) => [a] -> (a -> Set a) -> a -> Int
numberCongruenceClasses reps classes a = fromJust $ elemIndex (elemAt 0 (classes a)) reps

flatten :: [(a, a)] -> [a]
flatten = foldl (\acc (e1, e2) -> e1:e2:acc) []

relation = [(2,1), (3,1), (4,5)]
-}
