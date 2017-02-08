{-# LANGUAGE FlexibleInstances #-}
module FunctionLifting where

--type MonoFunction = forall a . (Num a) => a -> a

instance (Num a) => Num (a->a) where
    f + g = \x -> f x + g x
    f * g = \x -> f x * g x
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = const . fromIntegral
