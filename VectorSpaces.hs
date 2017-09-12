{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
import GHC.Float (float2Double)
import Control.Monad (liftM2)

class AbelianAddGroup g where
    gadd :: g -> g -> g
    gzero :: g
    gneg :: g -> g

class AbelianMulGroup g  where
    gmul :: g -> g -> g
    gone :: g
    ginv :: g -> g

class Field f where
    fadd :: f -> f -> f
    fmul :: f -> f -> f
    fneg :: f -> f
    finv :: f -> f
    fzero :: f
    fone :: f

instance (AbelianAddGroup f, AbelianMulGroup f) => Field f where
    fadd = gadd
    fmul = gmul
    fneg = gneg
    finv = ginv
    fzero = gzero
    fone = gone

class (AbelianAddGroup v, Field f) => VectorSpace f v where
    smul :: f -> v -> v

data Vector3 a = V3 a a a deriving Show

instance AbelianAddGroup Float where
    gadd = (+)
    gneg x = -x
    gzero = 0
instance AbelianMulGroup Float where
    gmul = (*)
    ginv x = 1/x
    gone = 1

instance AbelianAddGroup Double where
    gadd = (+)
    gzero = 0
    gneg x = -x

instance AbelianMulGroup Double where
    gmul = (*)
    ginv x = 1/x
    gone = 1

instance (AbelianAddGroup f) => AbelianAddGroup (Vector3 f) where
    gzero = V3 gzero gzero gzero
    gadd (V3 a b c) (V3 d e f) = V3 (gadd a d) (gadd b e) (gadd c f)
    gneg (V3 a b c) = V3 (gneg a) (gneg b) (gneg c)

instance VectorSpace Float (Vector3 Double) where
    smul x (V3 a b c) = let lambda = float2Double x in V3 (lambda*a) (lambda*b) (lambda*c)

instance (AbelianAddGroup g) => AbelianAddGroup (r -> g) where
    gadd = liftM2 gadd
    gzero = const gzero
    gneg = fmap (gneg)

instance VectorSpace Double (Double -> Double) where
    smul x = ((*x) . )
