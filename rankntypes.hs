{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE Impredicative #-}

import Control.Monad.State

myID :: forall a . a -> a
myID = id

pairwiseAutomorphism :: forall a b . (forall c . c -> c) -> (a, b) -> (a, b)
pairwiseAutomorphism f (a, b) = (f a, f b)

pairwiseTerminalMorphism :: forall a b d . (forall c . c -> d) -> (a, b) -> (d, d)
pairwiseTerminalMorphism f (a, b) = (f a, f b)

pairwiseFunctor :: forall a b f . (forall c . c ->f c) -> (a, b) -> (f a, f b)
pairwiseFunctor f (a, b) = (f a, f b)

{-
type Nat a b = forall c . a c -> b c

listToMaybe :: Nat [] Maybe
listToMaybe [] = Nothing
listToMaybe (a:as) = Just a

maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]


newtype Predicate = (a -> Bool)



double = \x->x*2 
-}



newtype ShowBox = SB (forall b . (forall a . (Show a) => a -> b) -> b)

makeShowBox :: (Show a) => a -> ShowBox
makeShowBox showable = SB (\f -> f showable)

applyShowBox :: ShowBox -> (forall a . (Show a) => a -> b) -> b
applyShowBox (SB showbox) f = showbox f

instance Show ShowBox where
    show sb = applyShowBox sb show

    
newtype PureShowBox = PSB (forall b . (forall a . (a, a -> String) -> b) -> b)

makePureShowBoxFromShowable :: (Show a) => a -> PureShowBox
makePureShowBoxFromShowable showable = PSB (\f -> f (showable, show))

applyPureShowBox :: PureShowBox -> (forall a . (a, a -> String) -> b) -> b
applyPureShowBox (PSB showbox) f = showbox f

instance Show PureShowBox where
    show psb = applyPureShowBox psb (\(a, showfunc) -> showfunc a)


data SQueue = SQ (forall t r . (forall a . (Queue a) => a t -> r) -> r)

newSQueueList :: SQueue
newSQueueList = SQ (\f -> f [])

newSQueueTS :: SQueue
newSQueueTS = SQ (\f -> f (empty :: TwoStacks t))

runOnSQ :: SQueue -> (forall a . (Queue a) => a t -> r) -> r
runOnSQ (SQ cont) f = cont f


church :: Int -> (a -> a) -> a -> a
church 0 _ x = x
church n f x = f (church (n-1) f x)

-- f:: forall a . (Queue a) => a -> t
-- empty :: (Queue a) => a
-- f empty :: t
class (Queue a) where
    enqueue :: a t -> t -> a t
    dequeue :: a t -> (t, a t)
    empty :: a t
    len :: a t -> Int
    

instance Queue [] where
    enqueue l t = l ++ [t]
    dequeue l = (head l, tail l)
    empty = []
    len = Prelude.length

data TwoStacks t = TS [t] [t]

instance Queue TwoStacks where
    enqueue (TS a b) t = TS b (t:a)
    dequeue (TS [] b) = let newa = reverse b in (head newa, TS (tail newa) [])
    dequeue (TS a b) = (head a, TS (tail a) b)
    empty = TS [] []
    len (TS a b) = Prelude.length a + Prelude.length b
    
    
    
    
--Object is defined as a type with
-- a) underlying representation
--      tuple of data
-- b) instance methods
--      way to create the type of all instance methods given the underlying representation

newtype Object instancemethods = Object (forall result . ((forall underlyingrep . (underlyingrep, instancemethods underlyingrep) -> result) -> result))

objLift :: forall instancemethods result . (forall underlyingrep . (underlyingrep, instancemethods underlyingrep) -> result) -> (Object instancemethods) -> result
objLift f (Object o) = o f

makeConstructorFromInstanceMethods :: instancemethods a -> a -> Object instancemethods
makeConstructorFromInstanceMethods instancemethods = \a -> Object (\f -> f (a, instancemethods))


newtype PrintableInstanceMethods a = PIM (a -> String)
newtype Printable = Printable (Object PrintableInstanceMethods)

makePrintableFromShowable :: (Show a) => a -> Printable
makePrintableFromShowable a = Printable $ makeConstructorFromInstanceMethods (PIM show) a

instance Show Printable where
    show (Printable obj) = objLift (\(rep, PIM toShow) -> toShow rep) obj
    

{- Equivalent Java code

interface Printable {
    String toString();
}

class FromShowable : Printable {
    Object underlyingRep; //Using Object here because Java object implements toString, like Show
    FromShowable(Object o) {
        underlyingRep = o;
    }
    String toString() {
        return underlyingRep.toString();
    }
}

-}




