{-# LANGUAGE FunctionalDependencies, KindSignatures #-}
import Control.Arrow
import Control.Category hiding ((.))
import qualified Control.Category as C
import Control.Monad

class (Category c, Category d) => CFunctor f c d | f -> c d where
    cfmap :: c a b -> d (f a) (f b)
    

class (Monoid a, Monoid b) => Homomorphism a b where
    applyHomomorphism :: a -> b
    {-
        Satisfying
        1) applyHomomorphism mzero = mzero
        2) applyHomomorphism (madd a b) = madd (applyHomomorphism a) (applyHomomorphism b)
    -}


