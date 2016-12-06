-- Pure FuncSet
module FuncSet (FuncSet,
    member, empty, insert, singleton,
    notMember,
    union, disjointUnion, intersection, difference,
    fromFunction, fromList, fromHashSet,
    partition) where

import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Data.Functor.Contravariant

newtype FuncSet a = FS (a->Bool)

member :: a -> FuncSet a -> Bool
member a (FS f) = f a

notMember :: a -> FuncSet a -> Bool
notMember = (.) not . member

union :: FuncSet a -> FuncSet a -> FuncSet a
union (FS f) (FS g) = FS (\a->f a || g a)

disjointUnion :: FuncSet a -> FuncSet b -> FuncSet (Either a b)
disjointUnion (FS f) (FS g) = FS (\a-> case a of Left l -> f l; Right r -> g r)

intersection :: FuncSet a -> FuncSet a -> FuncSet a
intersection (FS f) (FS g) = FS (\a->f a && g a)

difference :: FuncSet a -> FuncSet a -> FuncSet a
difference (FS f) (FS g) = FS (\a->f a && not (g a))

insert :: (Eq a) => FuncSet a -> a -> FuncSet a
insert (FS f) a = FS (\x -> x==a || f a)

empty :: FuncSet a
empty = FS (const False)

fromFunction :: (a->Bool) -> FuncSet a
fromFunction = FS

fromList :: (Eq a) => [a] -> FuncSet a
fromList = FS . flip elem

fromHashSet :: (Hashable a, Eq a) => HS.HashSet a -> FuncSet a
fromHashSet = FS . flip HS.member

singleton :: (Eq a) => a -> FuncSet a
singleton a = FS (\x->x==a)

partition :: (a -> Bool) -> FuncSet a -> (FuncSet a, FuncSet a) 
partition p s = (intersection s (FS p), difference s (FS p))

instance Contravariant FuncSet where 
    --contramap :: (a->b) -> FuncSet b -> FuncSet a
    contramap f (FS mem) = FS (mem . f)
--map is computationally impossible
--map f s is to create a set s' that asks, for if e' is a member of s'
--does there exist an element e in s such that f(e) is e'
--Which requires checking every element of s,
--which might have an infinite number of elements

