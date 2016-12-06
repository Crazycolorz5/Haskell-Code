import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import qualified Data.HashSet as HS
data Set a = FuncSet (a->Bool) | Hashed (HashSet a) | Both (HashSet a) (a->Bool)

member :: (Hashable a, Eq a) => Set a -> a -> Bool
member (FuncSet f) a = f a
member (Hashed s) a = HS.member s a
member (Both s f) a = HS.member s a || f a

union :: (Hashable a, Eq a) => Set a -> Set a -> Set a
union (FuncSet f) (FuncSet g) = FuncSet (\a -> f a || g a)
union (Hashed s) (Hashed r) = Hashed (HS.union s r)
union (FuncSet f) (Hashed s) = Both s f
union (Both s f) (FuncSet g) = Both s (\a->f a || g a)
union (Both s f) (Hashed r) = Both (HS.union s r) f
union (Both s f) (Both r g) = Both (HS.union s r) (\a->f a || g a)
union a b = union b a

empty :: Set a
empty = FuncSet (const False)

fromFunction :: (a -> Bool) -> Set a
fromFunction f = FuncSet f
